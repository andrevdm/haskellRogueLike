{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Levels.Level01 where

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

mkLevel :: Text -> Level
mkLevel mapData = Level { _lvlName = "L01"
                        , _lvlBoot = bootLevel 
                        , _lvlMapText = mapData
                        , _lvlTryMove = tryMove
                        }

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

tryMove :: [Actor] -> Maybe E.EntityType -> World -> WorldPos -> Actor -> [RogueAction]
tryMove destActors destEntityType _ posTo movingActor =
  -- Is the move allowed
  case (destActors, destEntityType) of
    ([], Just E.Blank) -> [ActMoveActor movingActor posTo]
    ([], Just E.Door) -> [ActMoveActor movingActor posTo]
    ([], Nothing) -> [ActMoveActor movingActor posTo]
    (_, Just E.Stairs) -> [ActGotoLevel Levels02]
    _ -> []
