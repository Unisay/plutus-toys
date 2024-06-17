{-
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}
-}
module AuctionValidatorBlueprint (
  writeBlueprintToFile,
)
where

import PlutusTx.Blueprint (
  ArgumentBlueprint (..),
  AsDefinitionId (..),
  ContractBlueprint (..),
  ParameterBlueprint (..),
  PlutusVersion (..),
  Preamble (..),
  Purpose (..),
  ValidatorBlueprint (..),
  definitionRef,
  deriveDefinitions,
  writeBlueprint,
 )

import AuctionValidator (
  AuctionDatum (..),
  AuctionParams (..),
  AuctionRedeemer (..),
  auctionUntypedValidator,
  auctionValidatorScriptCBOREncoded,
 )
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import PlutusLedgerApi.V3 (BuiltinData, ScriptContext, UnsafeFromData (..))
import PlutusTx.Blueprint.TH (makeIsDataSchemaIndexed)
import PlutusTx.Lift (makeLift)
import PlutusTx.Prelude (check)

auctionContractBlueprint :: ContractBlueprint
auctionContractBlueprint =
  MkContractBlueprint
    { contractId = Just "auction-contract"
    , contractPreamble = auctionPreamble -- defined below
    , contractValidators = Set.singleton auctionValidator -- defined below
    , contractDefinitions =
        deriveDefinitions @[AuctionParams, AuctionDatum, AuctionRedeemer]
    }

auctionPreamble :: Preamble
auctionPreamble =
  MkPreamble
    { preambleTitle = "Auction Contract"
    , preambleDescription = Just "A simple auction contract"
    , preambleVersion = "1.0.0"
    , preamblePlutusVersion = PlutusV2
    , preambleLicense = Just "MIT"
    }
deriving stock instance (Generic AuctionParams)
deriving stock instance (Generic AuctionRedeemer)
deriving stock instance (Generic AuctionDatum)
deriving anyclass instance (AsDefinitionId AuctionParams)
deriving anyclass instance (AsDefinitionId AuctionRedeemer)
deriving anyclass instance (AsDefinitionId AuctionDatum)

auctionValidator :: ValidatorBlueprint [AuctionParams, AuctionDatum, AuctionRedeemer]
auctionValidator =
  MkValidatorBlueprint
    { validatorTitle = "Auction Validator"
    , validatorDescription = Just "Auction validator"
    , validatorParameters =
        [ MkParameterBlueprint
            { parameterTitle = Just "Auction Validator Parameters"
            , parameterDescription = Just "Compile-time validator parameters"
            , parameterPurpose = Set.singleton Spend
            , parameterSchema = definitionRef @AuctionParams
            }
        ]
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Auction Redeemer"
          , argumentDescription = Just "Auction redeemer"
          , argumentPurpose = Set.fromList [Spend, Mint]
          , argumentSchema = definitionRef @AuctionRedeemer
          }
    , validatorDatum =
        Just
          MkArgumentBlueprint
            { argumentTitle = Just "Auction Datum"
            , argumentDescription = Just "Auction datum"
            , argumentPurpose = Set.singleton Spend
            , argumentSchema = definitionRef @AuctionDatum
            }
    , validatorCompiledCode = Just auctionValidatorScriptCBOREncoded
    }

writeBlueprintToFile :: FilePath -> IO ()
writeBlueprintToFile path = writeBlueprint path auctionContractBlueprint
