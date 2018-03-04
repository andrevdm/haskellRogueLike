{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Protolude 
import qualified Data.Text.IO as Txt
  
import qualified GameCore as GC
import qualified GameEngine as GE
import qualified Levels.Level01 as L01
import qualified Levels.Level02 as L02

main :: IO ()
main = do
  map01 <- Txt.readFile "worlds/simple.csv"
  map02 <- Txt.readFile "worlds/level02.csv"
  GE.runGame (getLevel map01 map02)

getLevel :: Text -> Text -> GC.Levels -> GC.Level
getLevel map01 _ GC.Levels01 = GC.Level { GC._lvlName = "L01"
                                        , GC._lvlBoot = L01.bootLevel 
                                        , GC._lvlMapText = map01
                                        }
getLevel _ map02 GC.Levels02 = GC.Level { GC._lvlName = "L02"
                                        , GC._lvlBoot = L02.bootLevel 
                                        , GC._lvlMapText = map02
                                        }


