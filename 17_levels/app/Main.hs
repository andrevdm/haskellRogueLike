{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Protolude 
  
import qualified GameCore as GC
import qualified GameEngine as GE
import qualified Levels.Level01 as L01

main :: IO ()
main = GE.runGame getLevel

getLevel :: GC.Levels -> GC.Level
getLevel GC.Levels01 = GC.Level { GC._lvlName = "L01"
                                , GC._lvlBoot = L01.bootLevel 
                                }


