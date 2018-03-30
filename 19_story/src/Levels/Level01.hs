{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Levels.Level01 (mkLevel) where

import Protolude hiding (Map)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified System.Random as Rnd
import           Control.Lens ((^.), (.~))

import qualified Memory as M
import           GameCore
import qualified Entities as E
import qualified EntityType as E
import qualified BoundedInt as B
import qualified UtilityBrain as UB

{-! SECTION< 19_01_mkLevel !-}
mkLevel :: Text -> Level
mkLevel mapText =
  Level { _lvlName = "L01"
        , _lvlBoot = bootLevel 
        , _lvlMapText = mapText
        , _lvlStoryHandler = storyWaitingForKey
        }
{-! SECTION> 19_01_mkLevel !-}

bootLevel :: World -> World
bootLevel w1 =
  let
    bug = mkEnemyActor "bug1" E.Bug (6, -2) & acUtilities .~ [UB.utilityOfInfatuation, UB.utilityOfWander, UB.utilityOfWanderToExit]
                                            & acDisposition .~ Disposition { _dsSmitten = 0.8
                                                                           , _dsWanderlust = 0.35
                                                                           , _dsWanderlustToExits = 0.4
                                                                           , _dsSmittenWith = [E.Player]
                                                                           }
          
    snake = mkEnemyActor "snake1" E.Snake (8, -4) & acUtilities .~ [UB.utilityOfWander, UB.utilityOfWanderToExit]
                                                  & acDisposition .~ Disposition { _dsSmitten = 0
                                                                                 , _dsWanderlust = 0.35
                                                                                 , _dsWanderlustToExits = 0.4
                                                                                 , _dsSmittenWith = []
                                                                                 }

    w2 = w1 & wdActors .~ Map.fromList [ (bug ^. acId, bug)
                                       , (snake ^. acId, snake)
                                       ]
  in
  w2

  where
    mkEnemyActor aid e (x, y) =
      Actor { _acId = Aid aid
            , _acClass = ClassEnemy
            , _acEntity = E.getEntity e
            , _acWorldPos = WorldPos (x, y)
            , _acStdGen = snd $ Rnd.split (w1 ^. wdPlayer ^. plActor ^. acStdGen)
            , _acFovDistance = 2
            , _acFov = Nothing
            , _acFovHistory = Set.empty
            , _acSkipMove = False
            , _acMoveEnergyCost = 150
            , _acEnergy = B.new 180 100
            , _acUtilities = []
            , _acDisposition = UB.emptyDisposition 
            , _acPosMemory = M.empty
            , _acProps = Map.empty
            }


{-! SECTION< 19_storyCommon !-}
storyCommon :: World -> RogueEvent -> [RogueAction]
storyCommon world evt =
  case evt of
    EvtMove destActors destEntityType posTo movingActor ->
      let isPlayer = isPlayerMoving world movingActor in
      case (isPlayer, destActors, destEntityType) of
        (_, [], Just E.Blank) -> [ActMoveActor movingActor posTo]
        (_, [], Just E.Door) -> [ActMoveActor movingActor posTo]
        (_, [], Nothing) -> [ActMoveActor movingActor posTo]

        -- Only the player can pickup potions. Set lights on
        (True, [], Just E.PotionLight) -> [ ActMoveActor movingActor posTo
                                          , ActSetPlayerProp "debug:light" "on"
                                          , ActRemoveEntity E.PotionLight posTo
                                          ]

        -- Only the player can pickup potions. Set lights off
        (True, [], Just E.PotionDark) -> [ ActMoveActor movingActor posTo
                                         , ActClearPlayerProp "debug:light"
                                         , ActRemoveEntity E.PotionDark posTo
                                         ]
        _ -> []
{-! SECTION> 19_storyCommon !-}


{-! SECTION< 19_storyWaitingForKey !-}
storyWaitingForKey :: World -> RogueEvent -> [RogueAction]
storyWaitingForKey world evt =
  case evt of
    EvtMove destActors destEntityType posTo movingActor ->
      let isPlayer = isPlayerMoving world movingActor in

      case (isPlayer, destActors, destEntityType) of
        -- Player picked up the key
        (True, [], Just E.Key) ->
          [ ActMoveActor movingActor posTo
          , ActSetStoryHandler storyDoorOpen
          , ActRemoveEntity E.Key posTo
          ] <>
          -- Replace all closed doors with open ones
          ((\closedDoorAt -> ActReplaceEntity E.DoorClosed closedDoorAt $ E.getEntity E.Door) <$> findPos E.DoorClosed)
                 
        _ -> storyCommon world evt

  where
    findPos :: E.EntityType -> [WorldPos]
    findPos et =
      let
        es = Map.toList $ world ^. wdMap
        found = filter (\(_, e) -> e ^. enType == et) es
      in
      fst <$> found
{-! SECTION> 19_storyWaitingForKey !-}

  
{-! SECTION< 19_storyDoorOpen !-}
storyDoorOpen :: World -> RogueEvent -> [RogueAction]
storyDoorOpen world evt =
  case evt of
    EvtMove destActors destEntityType posTo movingActor ->
      case (destActors, destEntityType) of
        ([], Just E.Key) -> [ActMoveActor movingActor posTo]
        (_, Just E.Stairs) -> [ActGotoLevel Levels02]
        _ -> storyCommon world evt
{-! SECTION> 19_storyDoorOpen !-}


{-! SECTION< 19_isPlayerMoving !-}
isPlayerMoving :: World -> Actor -> Bool
isPlayerMoving w a =
  w ^. wdPlayer ^. plActor ^. acId == a ^. acId
{-! SECTION> 19_isPlayerMoving !-}
