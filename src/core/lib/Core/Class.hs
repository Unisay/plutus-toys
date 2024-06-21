{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Typeclasses for blockchain operations
module Core.Class where

import Cardano.Api (
  SubmitResult (..),
  TxInMode (..),
  submitTxToNodeLocal,
 )
import Cardano.Api qualified as C
import Cardano.Api.Shelley (
  LedgerProtocolParameters (..),
  LocalNodeConnectInfo (..),
  NetworkId,
  PaymentCredential,
  SlotNo,
  Tx,
  TxId,
  TxIn,
 )
import Cardano.Slotting.Time (
  SlotLength,
  SystemStart,
 )
import Control.Monad.Except (MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..), ask, asks)
import Control.Monad.Trans.Except (ExceptT (..))
import Core.Utils (slotToUtcTime)
import Data.Aeson (FromJSON, ToJSON)
import Data.Set (Set)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Ouroboros.Consensus.HardFork.History (
  interpretQuery,
  slotToSlotLength,
 )
import Ouroboros.Network.Protocol.LocalStateQuery.Type (Target (..))
import Prettyprinter (
  Pretty (..),
  (<+>),
 )

-- | Error message obtained when a transaction was not accepted by the node.
newtype SendTxFailed = SendTxFailed {unSendTxFailed :: String}
  deriving stock (Eq, Ord, Show)

instance Pretty SendTxFailed where
  pretty (SendTxFailed msg) = "sendTx: Submission failed:" <+> pretty msg

class (Monad m) => MonadBlockchain m where
  sendTx :: Tx C.BabbageEra -> m (Either SendTxFailed TxId)
  utxoByTxIn :: Set TxIn -> m (C.UTxO C.BabbageEra)
  querySystemStart :: m SystemStart
  queryEraHistory :: m C.EraHistory
  querySlotNo :: m (SlotNo, SlotLength, UTCTime)
  queryProtocolParameters :: m (LedgerProtocolParameters C.BabbageEra)
  networkId :: m NetworkId

data MonadBlockchainError e
  = MonadBlockchainError e
  | FailWith String
  deriving stock (Functor, Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

{- Note [MonadUtxoQuery design]

The 'MonadUtxoQuery' class provides a lookup function that tells us the
unspent transaction outputs locked by one of a set of payment credentials.

The reason why this is not part of 'MonadBlockchain' is that the latter can
be implemented efficiently using only a running cardano-node, while 'MonadUtxoQuery'
requires a separate indexer. The classes are split to give callers more fine-grained
control over the capabilities they require.

-}

{- | A capability typeclass that provides methods for querying a chain indexer.
  See note [MonadUtxoQuery design].
  NOTE: There are currently no implementations of this class.
-}
class (Monad m) => MonadUtxoQuery m where
  -- | Given a set of payment credentials, retrieve all UTxOs associated with
  -- those payment credentials according to the current indexed blockchain state.
  utxosByPaymentCredentials :: Set PaymentCredential -> m (C.UTxO C.BabbageEra)

newtype MonadBlockchainCardanoNodeT e m a where
  MonadBlockchainCardanoNodeT
    :: { unMonadBlockchainCardanoNodeT :: ReaderT LocalNodeConnectInfo (ExceptT (MonadBlockchainError e) m) a
       }
    -> MonadBlockchainCardanoNodeT e m a
  deriving newtype (Functor, Applicative, Monad, MonadIO)

runQuery
  :: (MonadIO m)
  => C.QueryInMode a
  -> MonadBlockchainCardanoNodeT e m a
runQuery query = MonadBlockchainCardanoNodeT $ do
  nodeConnInfo <- ask
  liftIO (C.queryNodeLocalState nodeConnInfo VolatileTip query) >>= \case
    Left err -> throwError $ FailWith $ "runquery failed with error: " <> show err
    Right r -> pure r
runQuery'
  :: (MonadIO m, Show e1)
  => C.QueryInMode (Either e1 a)
  -> MonadBlockchainCardanoNodeT e2 m a
runQuery' query =
  runQuery query >>= \case
    Left err -> MonadBlockchainCardanoNodeT $ do
      let msg = "runQuery': Era mismatch: " <> show err
      throwError $ FailWith msg
    Right r -> pure r

runMonadBlockchainCardanoNodeT
  :: LocalNodeConnectInfo
  -> MonadBlockchainCardanoNodeT e m a
  -> m (Either (MonadBlockchainError e) a)
runMonadBlockchainCardanoNodeT nodeConnInfo (MonadBlockchainCardanoNodeT action) =
  runExceptT (runReaderT action nodeConnInfo)

instance (MonadIO m) => MonadBlockchain (MonadBlockchainCardanoNodeT e m) where
  sendTx tx = MonadBlockchainCardanoNodeT $ do
    nodeConnInfo <- ask
    result <-
      liftIO $
        submitTxToNodeLocal
          nodeConnInfo
          (TxInMode C.ShelleyBasedEraBabbage tx)
    case result of
      SubmitSuccess -> pure $ Right $ C.getTxId (C.getTxBody tx)
      SubmitFail reason -> do
        let msg = SendTxFailed (show reason)
        -- TODO impl logging here
        pure (Left msg)

  -- utxoByTxIn :: Set TxIn -> m (UTxO BabbageEra)
  utxoByTxIn txins =
    runQuery'
      ( C.QueryInEra
          (C.QueryInShelleyBasedEra C.ShelleyBasedEraBabbage (C.QueryUTxO (C.QueryUTxOByTxIn txins)))
      )

  -- querySystemStart :: m SystemStart
  querySystemStart = runQuery C.QuerySystemStart

  -- queryEraHistory :: m EraHistory
  queryEraHistory = runQuery C.QueryEraHistory

  -- querySlotNo :: m (SlotNo, SlotLength, UTCTime)
  querySlotNo = do
    (eraHistory@(C.EraHistory interpreter), systemStart) <- (,) <$> queryEraHistory <*> querySystemStart
    slotNo <-
      runQuery C.QueryChainPoint >>= \case
        C.ChainPointAtGenesis -> pure $ fromIntegral (0 :: Integer)
        C.ChainPoint slot _hsh -> pure slot
    MonadBlockchainCardanoNodeT $ do
      let logErr err = do
            let msg = "querySlotNo: Failed with " <> err
            -- TODO logging here
            throwError $ FailWith msg
      utctime <- either logErr pure (slotToUtcTime eraHistory systemStart slotNo)
      either
        (logErr . show)
        (\l -> pure (slotNo, l, utctime))
        (interpretQuery interpreter $ slotToSlotLength slotNo)

  -- queryProtocolParameters :: MonadIO m =>  m (LedgerProtocolParameters BabbageEra)
  queryProtocolParameters =
    LedgerProtocolParameters
      <$> runQuery'
        (C.QueryInEra (C.QueryInShelleyBasedEra C.ShelleyBasedEraBabbage C.QueryProtocolParameters))

  networkId :: MonadBlockchainCardanoNodeT e m NetworkId
  networkId = MonadBlockchainCardanoNodeT (asks localNodeNetworkId)
