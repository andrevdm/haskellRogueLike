{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Levels.Level02 where

import           Protolude

import           GameCore
import qualified EntityType as E

mkLevel :: Text -> Level
mkLevel mapData = Level { _lvlName = "L02"
                        , _lvlBoot = bootLevel 
                        , _lvlMapText = mapData
                        , _lvlTryMove = tryMove
                        }

bootLevel :: World -> World
bootLevel w = w


tryMove :: [Actor] -> Maybe E.EntityType -> World -> WorldPos -> Actor -> [RogueAction]
tryMove destActors destEntityType _ posTo movingActor =
  -- Is the move allowed
  case (destActors, destEntityType) of
    ([], Just E.Blank) -> [ActMoveActor movingActor posTo]
    ([], Just E.Door) -> [ActMoveActor movingActor posTo]
    ([], Nothing) -> [ActMoveActor movingActor posTo]
    _ -> []
