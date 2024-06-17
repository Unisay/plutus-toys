{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Startup cardano nodes programmatically
module Devnet.CardanoNode where

import Cardano.Api (
  ConsensusModeParams (..),
  Env (..),
  EpochSlots (..),
  InitialLedgerStateError,
  LocalNodeConnectInfo (..),
  NetworkId (Mainnet, Testnet),
  NetworkMagic (..),
  envSecurityParam,
 )
import Cardano.Api qualified as CAPI
import Cardano.Chain.Genesis qualified
import Cardano.Crypto (
  RequiresNetworkMagic (..),
  getProtocolMagic,
 )
import Control.Monad.Except (
  MonadError,
  throwError,
 )
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (runExceptT)
import Data.SOP.Strict (NP ((:*)))
import Ouroboros.Consensus.Cardano.CanHardFork qualified as Consensus
import Ouroboros.Consensus.HardFork.Combinator qualified as Consensus
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras qualified as HFC
import Ouroboros.Consensus.HardFork.Combinator.Basics qualified as HFC

-- | Load the node config file and create 'LocalNodeConnectInfo' and 'Env' values that can be used to talk to the node.
loadConnectInfo
  :: (MonadError InitialLedgerStateError m, MonadIO m)
  => FilePath
  -- ^ Node config file (JSON)
  -> FilePath
  -- ^ Node socket
  -> m (LocalNodeConnectInfo, Env)
loadConnectInfo nodeConfigFilePath socketPath = do
  (env, _) <-
    liftIO (runExceptT (CAPI.initialLedgerState (CAPI.File nodeConfigFilePath)))
      >>= either throwError pure

  -- Derive the NetworkId as described in network-magic.md from the
  -- cardano-ledger-specs repo.
  let byronConfig =
        (\(Consensus.WrapPartialLedgerConfig (Consensus.ByronPartialLedgerConfig bc _) :* _) -> bc)
          . HFC.getPerEraLedgerConfig
          . HFC.hardForkLedgerConfigPerEra
          $ envLedgerConfig env

      networkMagic =
        getProtocolMagic $
          Cardano.Chain.Genesis.configProtocolMagic byronConfig

      networkId = case Cardano.Chain.Genesis.configReqNetMagic byronConfig of
        RequiresNoMagic -> Mainnet
        RequiresMagic -> Testnet (NetworkMagic networkMagic)

      cardanoModeParams = CardanoModeParams . EpochSlots $ 10 * envSecurityParam env

  -- Connect to the node.
  let connectInfo :: LocalNodeConnectInfo
      connectInfo =
        LocalNodeConnectInfo
          { localConsensusModeParams = cardanoModeParams
          , localNodeNetworkId = networkId
          , localNodeSocketPath = CAPI.File socketPath
          }
  pure (connectInfo, env)
