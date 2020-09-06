module Main where

import qualified TypedNetwork as TN
import qualified ThreeLayerNetwork as TLN

main :: IO ()
main = do
    _ <- putStrLn "Running ThreeLayerNetwork"
    TLN.runMain
