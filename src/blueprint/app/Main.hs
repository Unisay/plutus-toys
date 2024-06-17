{-# LANGUAGE LambdaCase #-}

module Main where

---------------------
-- TODO, on hold pending this plutus PR:
-- https://github.com/IntersectMBO/plutus/pull/6165
---------------------
-- import AuctionValidatorBlueprint (writeBluePrintToFile)

import Control.Monad.Except (runExceptT)
import Devnet.CardanoNode (loadConnectInfo)

main :: IO ()
main = do
  runExceptT
    ( loadConnectInfo
        "/home/kayvan/dev/workspaces/workspace-schwarzer-swan/haskell/plutus-toys/cardano-configuration/network/sanchonet/cardano-node/config.json"
        "/home/kayvan/dev/workspaces/workspace-schwarzer-swan/haskell/plutus-toys/var/8.11.0-sancho/sancho/cardano-node-preview.socket"
    )
    >>= \case
      Left e -> print e
      Right{} -> pure ()

  putStrLn "Attempting to print Auciton plutus blueprint.json"

-- writeBluePrintToFile "./blueprint.json"
