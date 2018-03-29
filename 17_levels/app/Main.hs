{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Protolude 
import qualified Data.Text.IO as Txt
  
import qualified GameCore as GC
import qualified GameEngine as GE
import qualified Levels.Level01 as L01

main :: IO ()
main = do
  map01 <- Txt.readFile "worlds/simple.csv"
  GE.runGame (getLevel map01)

getLevel :: Text -> GC.Levels -> GC.Level
getLevel map01 GC.Levels01 = L01.mkLevel map01


