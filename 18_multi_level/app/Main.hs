{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Protolude 
import qualified Data.Text.IO as Txt
  
import qualified GameCore as GC
import qualified GameEngine as GE
import qualified Levels.Level01 as L01
import qualified Levels.Level02 as L02

{-! SECTION< 18_main !-}
main :: IO ()
main = do
  map01 <- Txt.readFile "worlds/simple.csv"
  map02 <- Txt.readFile "worlds/level02.csv"
  GE.runGame (getLevel map01 map02)

getLevel :: Text -> Text -> GC.Levels -> GC.Level
getLevel map01 _ GC.Levels01 = L01.mkLevel map01
getLevel _ map02 GC.Levels02 = L02.mkLevel map02
{-! SECTION> 18_main !-}
