{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module UtilityBrain ( selectTopUtility
                    , assessUtilities
                    , utilityOfInfatuation
                    , utilityOfWander
                    , utilityOfWanderToExit
                    , emptyDisposition
                    ) where

import Protolude 
import qualified Data.Map.Strict as Map
import qualified Data.List as Lst
import qualified Data.DList as DLst
import qualified System.Random as Rnd
import qualified Control.Arrow as Ar
import           Control.Lens
import           Control.Monad.Writer.Strict (tell, MonadWriter)

import qualified Memory as M
import           GameCore
import qualified EntityType as E


telld :: MonadWriter (DLst.DList a) m => a -> m ()
telld t = tell (DLst.singleton t)

path :: PathTo -> Path
path (PathToEntity p _ _) = p
path (PathToActor p _ _) = p
path (PathToPlayer p _ _) = p

pathPs :: Path -> [WorldPos]
pathPs (Path p) = p

selectTopUtility :: [(Float, Actor, Impulse, Text, Maybe PathTo)]
                 -> UtilAnnotator (Maybe (Float, Actor, Impulse, Text, Maybe PathTo))
selectTopUtility rs = do
  telld $ UeAt "select top"

  case rs of
      [] -> do
        telld $ UeSelectTopNone "No utilities"
        pure Nothing

      (u@(v0,a0,i0,n0,_):_) -> do
        telld $ UeSelectTopAbove (v0 - 0.001)

        -- Get the results with the top scores (fuzzy match)
        let top = Lst.takeWhile (\(v2,_,_,_,_) -> v2 >= v0 - 0.001) rs 

        -- Get a random index. Grab a StdGen from the first actor, and ignore the new stdgen, its not important here
        let ((idx, _), rndB) = 
              let (rndA, rndB') = Rnd.split (a0 ^. acStdGen) in
              (Rnd.randomR (0, length top - 1) rndA, rndB') 

        case atMay top idx of
          Just (s, a, i, n, p) -> do
            telld $ UeSelectTopOne s n i "selected"
            pure $ Just (s, a & acStdGen .~ rndB, i, n, p)

          Nothing -> do
            telld $ UeSelectTopOne v0 n0 i0 $ "Nothing at index, using zero: " <> show idx
            pure $ Just u

  
-- | See the docs on acUtilities
-- | Mainly that the world is threaded through utilities and any updates are kept even if no/other utilities are selected
-- | The actor in the results are speculative and only the actor for the selected utility gets used
assessUtilities :: [PathTo] -> World -> Actor -> UtilAnnotator ([(Float, Actor, Impulse, Text, Maybe PathTo)], World)
assessUtilities paths world actor = do
  (rs, wNext) <- foldM assess ([], world) (actor ^. acUtilities)
  ranked <- rankUtility rs
  pure (ranked, wNext)
  
  where
    assess (hist, w) u = do
      let a = fromMaybe actor $ w ^. wdActors ^.at (actor ^. acId) 
      (rs, wNext) <- u w a paths
      pure (hist <> rs, wNext)


rankUtility :: [(Float, Actor, Impulse, Text, Maybe PathTo)] -> UtilAnnotator [(Float, Actor, Impulse, Text, Maybe PathTo)]
rankUtility us = 
  pure . Lst.reverse $ Lst.sortOn (\(x, _, _, _, _) -> x) us


clamp :: Float -> Float
clamp = clampTo 0.0 1.0


clampTo :: Float -> Float -> Float -> Float
clampTo vmin vmax = min vmax . max vmin


onlyEntitiesOfType :: [E.EntityType] -> [PathTo] -> [PathTo]
onlyEntitiesOfType types =
  filter go
  where
    go (PathToEntity _ e _) = e ^. enType `elem` types
    go _ = False


emptyDisposition :: Disposition
emptyDisposition = Disposition { _dsSmitten = 0
                               , _dsWanderlust = 0
                               , _dsWanderlustToExits = 0
                               , _dsSmittenWith = []
                               } 


distanceToRange :: PathTo -> Int -> Maybe Float
distanceToRange pt fov =
  let (Path p) = path pt in
  case (p, Lst.reverse p) of
    ([], _) -> Nothing
    (_, []) -> Nothing
    (WorldPos (fx, fy) : _, WorldPos (tx, ty) : _) ->
      if fx == tx && fy == ty
      then
        Nothing
      else
        -- Distance to point
        let distance = sqrt . fromIntegral $ ((tx - fx) ^ 2) + ((ty - fy) ^ 2) in
        -- Max distance for fov, i.e. cartesian distance to a corner of the fov
        let maxDist = sqrt ((fromIntegral fov ** 2) * 2) in
        Just $ distance / maxDist
      

utilityOfWander :: World -> Actor -> [PathTo] -> UtilAnnotator ([(Float, Actor, Impulse, Text, Maybe PathTo)], World)
utilityOfWander world actor _paths = do
  telld $ UeAt "Wander"

  let rule = clamp $ 0.02 * (10 * clamp (actor ^. acDisposition ^. dsWanderlust)) 
  pure ([(rule, actor, ImpMoveRandom, "wander", Nothing)], world)


utilityOfWanderToExit :: World -> Actor -> [PathTo] -> UtilAnnotator ([(Float, Actor, Impulse, Text, Maybe PathTo)], World)
utilityOfWanderToExit world actor' allPaths = do
  telld $ UeAt "WanderToExit"
  telld . UeNote $ "at: " <> show (actor' ^. acWorldPos)
  telld . UeNote $ "goal: " <> show (M.recall "wanderToExit.goal" $ actor' ^. acPosMemory)
  telld . UeNote $ "avoid: " <> show (M.recall "wanderToExit.avoid" $ actor' ^. acPosMemory)
  
  let rule x = clamp $ 1 - (0.04 * x + (1.24 - clamp (actor' ^. acDisposition ^. dsWanderlustToExits))) 

  -- Find exits
  let goalPaths = onlyEntitiesOfType [E.Door] allPaths 

  let seek actor paths = do
        -- Normalise distances
        let pathsNormalisedMay = (\p -> (p, distanceToRange p (actor ^. acFovDistance))) <$> paths 
        let pathsNormalised = catNormalisedMay pathsNormalisedMay 
        -- Run utility calculation
        let results = Ar.second rule <$> pathsNormalised 
        let clampedResults = Ar.second clamp <$> results
        -- Result
        (\(p, score) -> (score, actor, ImpMoveTowards (path p), "wander to exit", Just p)) <$> clampedResults
  
  r <- seekGoal "wanderToExit" world actor' goalPaths seek
  telld . UeNote $ "res.len= " <> show (length r)
  pure r

  
utilityOfInfatuation :: World -> Actor -> [PathTo] -> UtilAnnotator ([(Float, Actor, Impulse, Text, Maybe PathTo)], World)
utilityOfInfatuation world actor allPaths = do
  telld . UeAt $ "Infatuation: " <> show (length allPaths) -- debugShowPathTos allPaths --show (actor ^. acDisposition ^. dsSmitten)

  let rule x = clamp $ -x ** 4 + clamp (actor ^. acDisposition ^. dsSmitten) 
  let clampedResults = moveTowardsUtil (actor ^. acDisposition ^. dsSmittenWith) rule allPaths actor

  let (r, _) = ((\(p, score) -> (score, actor, ImpMoveTowards (path p), "infatuation", Just p)) <$> clampedResults, world)
  let r2 = (\(f, _, _, _, _) -> showF f) <$> r
  telld . UeNote $ "infatuation: " <> show r2
  
  pure ((\(p, score) -> (score, actor, ImpMoveTowards (path p), "infatuation", Just p)) <$> clampedResults, world)


seekGoal :: Text     -- ^ prefix for memory storage
         -> World
         -> Actor    -- ^ the moving actor
         -> [PathTo] -- ^ filtered paths, any one of which is a valid goal
         -> (Actor -> [PathTo] -> [(Float, Actor, Impulse, Text, Maybe PathTo)]) -- ^ if there is no historic goal, these will be used
         -> UtilAnnotator ([(Float, Actor, Impulse, Text, Maybe PathTo)], World)
seekGoal memPrefix world actor paths scorePaths = do
  let keyGoal = memPrefix <> ".goal"
  let keyAvoid = memPrefix <> ".avoid"
  let ttlGoal = 50
  let ttlAvoid = 200

  -- is there already a goal, i.e. already a position the actor is heading towards
  case mapHeadMay $ M.recall keyGoal (actor ^. acPosMemory) of
    -- Already seeking a goal
    Just (goal, _) -> do
      telld . UeNote $ "seeking goal: " <> show goal
      
      --At the goal
      if goal == actor ^.acWorldPos
        then do
          telld . UeNote $ "at goal"

          --Forget the goal and try for next
          let actor' = actor & acPosMemory %~ M.remember const keyAvoid ttlAvoid goal
                             & acPosMemory %~ M.forgetAll keyGoal

          let world' = world & wdActors %~ Map.insert (actor ^. acId) actor'
          seekGoal memPrefix world' actor' (removePathsToAvoid keyAvoid actor' paths) scorePaths
        else do
          telld . UeNote $ "not at goal"

          -- Not at the goal, continue towards it
          -- Only consider paths to the goal
          let pathsToGoal' = filter (\p -> lastMay (pathPs . path $ p) == Just goal) paths 
          let pathsToGoal = removePathsToAvoid keyAvoid actor pathsToGoal' 

          selectTopUtility (scorePaths actor pathsToGoal) >>= \case
            Nothing -> seekGoal memPrefix world (actor & acPosMemory %~ M.forgetAll keyGoal) paths scorePaths

            Just (score', actor', impulse', name', pathToMay') -> do
              -- if moving to the goal
              let actorFinal = 
                    if Just goal == ((flip atMay 1 . pathPs . path) =<< pathToMay')
                    then
                      actor' & acPosMemory %~ M.remember const keyAvoid ttlAvoid goal
                             & acPosMemory %~ M.forgetAll keyGoal
                    else
                      actor'
              
              -- seek
              pure ([(score', actorFinal, impulse', name', pathToMay')], world)
    
    -- Not seeking a goal
    Nothing -> do
      telld . UeNote $ "not seeking goal"

      -- Score all the paths
      let newGoals = scorePaths actor $ removePathsToAvoid keyAvoid actor paths 
      -- Select the top scoring one
      selectTopUtility newGoals >>= \case
        Nothing -> pure ([], world) -- Nothing to find apparently

        Just (score', actor', impulse', name', pathToMay') ->

          -- The top scoring utility = our new goal
          case (pathToMay', (lastMay . pathPs . path) =<< pathToMay') of
            -- Path to the goal and the goal position
            (Just pathTo, Just goalPos) -> do
              let newActor = actor' & acPosMemory %~ M.remember const keyGoal ttlGoal goalPos . M.forgetAll keyGoal 
              pure ([(score', newActor, impulse', name', Just pathTo)], world)

            -- Nothing to find apparently
            _ -> pure ([], world)

  where
    mapHeadMay = headMay . Map.toList

    removePathsToAvoid :: Text -> Actor -> [PathTo] -> [PathTo]
    removePathsToAvoid keyAvoid a scored =
      let avoid = M.recall keyAvoid $ a ^. acPosMemory in
      let isInAvoid p = Map.member p avoid in
      let shouldInclude p = maybe True (not . isInAvoid) (lastMay p) in
      filter (\p -> shouldInclude (pathPs . path $ p)) scored


moveTowardsUtil :: [E.EntityType] -> (Float -> Float) -> [PathTo] -> Actor -> [(PathTo, Float)]
moveTowardsUtil es rule paths actor =
  let
    -- Find exits
    goalPaths = onlyEntitiesOfType es paths

      -- Normalise distances
    pathsNormalisedMay = (\p -> (p, distanceToRange p (actor ^. acFovDistance))) <$> goalPaths 
    pathsNormalised = catNormalisedMay pathsNormalisedMay 
    -- Run utility calculation
    results = Ar.second rule <$> pathsNormalised 
    clampedResults = Ar.second clamp <$> results
  in
    clampedResults


catNormalisedMay :: [(PathTo, Maybe float)] -> [(PathTo, float)]
catNormalisedMay ps =
  catMaybes $ go <$> ps

  where
    go (_, Nothing) = Nothing
    go (p, Just v) = Just (p, v) 
