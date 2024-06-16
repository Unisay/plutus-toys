module Main where

import AuctionValidatorBlueprint (writeBluePrintToFile)

main :: IO ()
main = do
    putStrLn "Attempting to print Auciton plutus blueprint.json"
    writeBluePrintToFile "./blueprint.json"
