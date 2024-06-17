{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}

module AuctionValidator (
  -- validator
  auctionTypedValidator,
  auctionUntypedValidator,
  auctionValidatorScript,
  auctionValidatorScriptCBOREncoded,
  -- data
  AuctionParams (..),
  AuctionDatum (..),
  AuctionRedeemer (..),
) where

import Data.ByteString (ByteString)
import Data.ByteString.Short (fromShort)
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V1 (Lovelace, POSIXTime, PubKeyHash, Value, from, unsafeFromBuiltinData)
import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import PlutusLedgerApi.V1.Interval (contains)
import PlutusLedgerApi.V1.Value (lovelaceValue)
import PlutusLedgerApi.V2 (Datum (Datum), OutputDatum (..), TxInfo (..), TxOut (..), to)
import PlutusLedgerApi.V2.Contexts (ScriptContext (..), getContinuingOutputs)
import PlutusTx (BuiltinData, CompiledCode, compile, liftCodeDef, unsafeApplyCode)
import PlutusTx qualified
import PlutusTx.Prelude qualified as PlutusTx
import PlutusTx.Show qualified as PlutusTx

data AuctionParams = AuctionParams
  { apSeller :: PubKeyHash
  , apAsset :: Value
  , apMinBid :: Lovelace
  , apEndTime :: POSIXTime
  }

PlutusTx.makeLift ''AuctionParams

data Bid = Bid
  { bBidder :: PubKeyHash
  , bAmount :: Lovelace
  }

PlutusTx.deriveShow ''Bid
PlutusTx.unstableMakeIsData ''Bid

instance PlutusTx.Eq Bid where
  {-# INLINEABLE (==) #-}
  bid == bid' =
    bBidder bid
      PlutusTx.== bBidder bid'
      PlutusTx.&& bAmount bid
      PlutusTx.== bAmount bid

newtype AuctionDatum = AuctionDatum {aHigestBid :: Maybe Bid}
PlutusTx.unstableMakeIsData ''AuctionDatum

data AuctionRedeemer = NewBid Bid | PayOut
PlutusTx.unstableMakeIsData ''AuctionRedeemer

{-# INLINEABLE auctionTypedValidator #-}
auctionTypedValidator
  :: AuctionParams
  -> AuctionDatum
  -> AuctionRedeemer
  -> ScriptContext
  -> Bool
auctionTypedValidator params (AuctionDatum highestBid) redeemer ctx@(ScriptContext txInfo _) =
  PlutusTx.and conditions
  where
    conditions :: [Bool]
    conditions = case redeemer of
      NewBid bid ->
        [ sufficientBid bid
        , validBidTime
        , refundPreviouseHighestBid
        , correctNewDatum bid
        ]
      PayOut ->
        [ validPayoutTime
        , sellerGetsHighestBid
        , highestBidderGetsAsset
        ]

    sufficientBid :: Bid -> Bool
    sufficientBid (Bid _ amount) = case highestBid of
      Just (Bid _ amount') -> amount PlutusTx.> amount'
      Nothing -> amount PlutusTx.> apMinBid params

    validBidTime :: Bool
    validBidTime = to (apEndTime params) `contains` txInfoValidRange txInfo

    refundPreviouseHighestBid :: Bool
    refundPreviouseHighestBid = case highestBid of
      Nothing -> True
      Just (Bid bidder amount) -> case PlutusTx.find
        ( \o ->
            txOutAddress o
              PlutusTx.== pubKeyHashAddress bidder
              PlutusTx.&& txOutValue o
              PlutusTx.== lovelaceValue amount
        )
        (txInfoOutputs txInfo) of
        Just _ -> True
        Nothing -> PlutusTx.traceError "Not found: refund output"

    correctNewDatum :: Bid -> Bool
    correctNewDatum bid = case getContinuingOutputs ctx of
      [o] -> case txOutDatum o of
        OutputDatum (Datum newDatum) -> case PlutusTx.fromBuiltinData newDatum of
          Just bid' ->
            PlutusTx.traceIfFalse
              ( "Invalid output datum: expected "
                  PlutusTx.<> PlutusTx.show bid
                  PlutusTx.<> ", but got "
                  PlutusTx.<> PlutusTx.show bid'
              )
              (bid PlutusTx.== bid')
          Nothing ->
            PlutusTx.traceError
              ( "Failed to decode output datum"
                  PlutusTx.<> PlutusTx.show newDatum
              )
        OutputDatumHash _ ->
          PlutusTx.traceError "Expected OutputDatum, got OutputDatumHash"
        NoOutputDatum ->
          PlutusTx.traceError "Expected OutputDatum, got NoOutputDatum"
      os ->
        PlutusTx.traceError
          ( "Expected exactly one continuing output, got "
              PlutusTx.<> PlutusTx.show (PlutusTx.length os)
          )

    validPayoutTime :: Bool
    validPayoutTime = from (apEndTime params) `contains` txInfoValidRange txInfo

    sellerGetsHighestBid :: Bool
    sellerGetsHighestBid = case highestBid of
      Nothing -> True
      Just (Bid _ amount) -> case PlutusTx.find
        ( \o ->
            txOutAddress o
              PlutusTx.== pubKeyHashAddress (apSeller params)
              PlutusTx.&& txOutValue o
              PlutusTx.== lovelaceValue amount
        )
        (txInfoOutputs txInfo) of
        Just _ -> True
        Nothing -> PlutusTx.traceError "Not found: Output paid to seller"

    highestBidderGetsAsset :: Bool
    highestBidderGetsAsset = case highestBid of
      Nothing -> True
      Just (Bid bidder _) ->
        case PlutusTx.find
          ( \o ->
              txOutAddress o
                PlutusTx.== pubKeyHashAddress bidder
                PlutusTx.&& txOutValue o
                PlutusTx.== apAsset params
          )
          (txInfoOutputs txInfo) of
          Just _ -> True
          Nothing -> PlutusTx.traceError "Not found: Output paid to highest bidder"

{-# INLINEABLE auctionUntypedValidator #-}
auctionUntypedValidator
  :: AuctionParams
  -> BuiltinData
  -> BuiltinData
  -> BuiltinData
  -> ()
auctionUntypedValidator params datum redeemer ctx =
  PlutusTx.check
    ( auctionTypedValidator
        params
        (unsafeFromBuiltinData datum)
        (unsafeFromBuiltinData redeemer)
        (unsafeFromBuiltinData ctx)
    )

appliedAuctionValidator
  :: AuctionParams
  -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
appliedAuctionValidator params =
  $$(compile [||auctionUntypedValidator||])
    `unsafeApplyCode` liftCodeDef params

auctionValidatorScript :: AuctionParams -> SerialisedScript
auctionValidatorScript = serialiseCompiledCode . appliedAuctionValidator

auctionValidatorScriptCBOREncoded :: AuctionParams -> ByteString
auctionValidatorScriptCBOREncoded = fromShort . auctionValidatorScript
