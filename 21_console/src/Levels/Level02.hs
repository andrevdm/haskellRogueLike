{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Levels.Level02 (mkLevel)  where

import           Protolude

import           GameCore
import qualified EntityType as E

  
mkLevel :: Text -> Level
mkLevel mapText =
  Level { _lvlName = "L02"
        , _lvlBoot = bootLevel 
        , _lvlMapText = mapText
        , _lvlStoryHandler = storySimple
        }


bootLevel :: World -> World
bootLevel w = w


storySimple :: World -> RogueEvent -> [RogueAction]
storySimple _ evt =
  case evt of
    EvtMove destActors destEntityType posTo movingActor ->
      case (destActors, destEntityType) of
        ([], Just E.Blank) -> [ActMoveActor movingActor posTo]
        ([], Just E.Door) -> [ActMoveActor movingActor posTo]
        ([], Nothing) -> [ActMoveActor movingActor posTo]
        _ -> []
