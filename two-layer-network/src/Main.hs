module Main where

import qualified TypedNetwork as TN
import qualified ThreeLayerNetwork as TLN
import qualified BaseNetwork as BN

main :: IO ()
main = do
    _ <- putStrLn "Running ThreeLayerNetwork"
    BN.runMain
