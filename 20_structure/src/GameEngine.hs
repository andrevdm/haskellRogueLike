{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GameEngine where

import Protolude hiding (Map)
import qualified Data.Set as Set
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.List as Lst
import qualified Data.List.Index as Lst
import qualified Data.DList as DLst
import qualified Data.Text as Txt
import qualified Data.Text.Encoding as TxtE
import qualified Data.Aeson.Text.Extended as Ae
import qualified Data.ByteString.Lazy as BSL
import qualified Codec.Compression.BZip as Bz
import qualified System.Random as Rnd
import           Control.Lens (at, _1, (^.), (.~), (%~))
import qualified Control.Arrow as Ar
import           Control.Monad.Writer.Strict (runWriter)
import           Control.Concurrent.STM (atomically, readTVar, newTVar, modifyTVar', TVar)
import           Control.Monad.Trans (lift)
import           Control.Monad.Reader (ask, runReaderT, ReaderT, MonadReader, MonadTrans)
import           Control.Concurrent.STM (TVar, modifyTVar', newTVar, atomically, readTVar, writeTVar)

import qualified Memory as M
import           GameCore
import qualified GameHost as Host
import           GameHost (conSendData, conReceiveText)
import qualified Entities as E
import qualified EntityType as E
import qualified BoundedInt as B
import qualified UtilityBrain as UB


----------------------------------------------------------------------------------------------------------------
-- L0 (setup)
----------------------------------------------------------------------------------------------------------------
runGame :: (Levels -> Level) -> IO ()
runGame getLevel = Host.runHost (manageConnection getLevel)


manageConnection :: (Levels -> Level) -> Host.Connection -> IO ()
manageConnection getLevel conn = do
  initCmd <- conn ^. conReceiveText 

  case parseCommand initCmd of
    Just ("init", cmdData) -> do
      std <- Rnd.getStdGen
      
      case initialiseConnection conn cmdData std getLevel of -- initialiseConnection is L3
        Right world -> do
          app <- atomically $ do
            worldV <- newTVar world
            connV <- newTVar conn
            pure AppState { appConnection = connV
                          , appWorld = worldV
                          }

          runReaderT (unAppT $ initialiseConfig >> startGame) app
 
        Left e ->
          runReaderT (sendHostError e) conn
        
    _ ->
      pass
----------------------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------------------
-- L1 (orchestration)
----------------------------------------------------------------------------------------------------------------
data AppState = AppState { appConnection :: !(TVar Host.Connection)
                         , appWorld :: !(TVar World)
                         } 

newtype AppT m a = AppT { unAppT :: ReaderT AppState m a 
                        } deriving (Functor, Applicative, Monad, MonadReader AppState, MonadTrans)

askApp :: (AppState -> TVar a) -> AppT IO a
askApp getter = do
  app <- ask
  lift . atomically . readTVar $ getter app

modifyApp :: (AppState -> TVar a) -> (a -> a) -> AppT IO ()
modifyApp getter modify' = do
  app <- ask
  lift . atomically $ modifyTVar' (getter app) modify'
----

initialiseConfig :: AppT IO ()
initialiseConfig = do
  conn <- askApp appConnection
  world <- askApp appWorld
  app <- ask
  lift $ runReaderT (sendConfig $ world ^. wdConfig) (conn, appWorld app)
  

startGame :: AppT IO ()
startGame = do
  app <- ask
  conn <- askApp appConnection

  lift . forever $ do
    t <- conn ^. conReceiveText
    r <- runReaderT (handleCommand t) (conn, appWorld app)

    case r of
      Nothing -> pass
      Just e -> putText e
----------------------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------------------
-- L2 (bridge / external services)
----------------------------------------------------------------------------------------------------------------
class (Monad m) => MonadHost m where
  receiveHostText :: m Text
  sendHostData :: Text -> m ()
  sendHostLog :: Text -> m ()
  sendHostError :: Text -> m ()
  sendHostUiConfig :: UiConfigData -> m ()
  compressData :: Text -> m BSL.ByteString

  --default implementation
  sendHostLog err = sendHostData $ Ae.encodeText $ UiMessage "log" err
  sendHostError err = sendHostData $ Ae.encodeText $ UiMessage "error" err
  sendHostUiConfig config = sendHostData . Ae.encodeText $ UiConfig "config" config
  compressData = pure . Bz.compress . BSL.fromStrict . TxtE.encodeUtf8 

instance MonadHost (ReaderT (Host.Connection, TVar World) IO) where
  receiveHostText = do
    (conn, _) <- ask
    lift $ conn ^. conReceiveText

  sendHostData t = do
    (conn, _) <- ask
    lz <- compressData t
    lift $ conn ^. conSendData $ lz

instance MonadHost (ReaderT Host.Connection IO) where
  receiveHostText = do
    conn <- ask
    lift $ conn ^. conReceiveText

  sendHostData t = do
    conn <- ask
    lz <- compressData t
    lift $ conn ^. conSendData $ lz


class (Monad m) => MonadWorld m where
  askWorld :: m World
  putWorld :: World -> m ()
  modifyWorld :: (World -> World) -> m ()
  debugPrint :: Text -> m ()

instance MonadWorld (ReaderT (Host.Connection, TVar World) IO) where
  askWorld = do
    (_, wt) <- ask
    w <- lift . atomically $ readTVar wt
    pure w
    
  putWorld w = do
    (_, wt) <- ask
    lift . atomically $ writeTVar wt w

  modifyWorld fn = do
    (_, wt) <- ask
    lift . atomically $ modifyTVar' wt fn
  
  debugPrint = putText
-------------------

handleCommand :: (MonadHost m, MonadWorld m) => Text -> m (Maybe Text)
handleCommand t = 
  case parseCommand t of
    Nothing -> pure . Just $ "error parsing: " <> t
    Just (cmd, cmdData) -> do
      runCmd cmd cmdData
      pure Nothing


sendConfig :: (MonadHost m) => Config -> m ()
sendConfig config =
  sendHostData . Ae.encodeText $ UiConfig "config" (buildConfig config)


runCmd :: (MonadHost m, MonadWorld m) => Text -> [Text] -> m ()
runCmd cmd cmdData = 
  case cmd of
    "redraw" -> 
      case parseScreenSize cmdData of
        Nothing -> sendHostError "missing / invalid screen size"
        Just (sx, sy) -> do
          updatePlayer (plScreenSize .~ (sx, sy))
          world <- askWorld
          sendHostData $ Ae.encodeText (drawAndSend world)
          sendHostLog "draw"
      
    "key" -> do
      -- Handle the key press
      modifyWorld (\w ->
                     -- Do the actions as if they will succeed
                     let pendingWorld = runActions w $ handleKey w cmdData in
                     -- Apply, if the move is allowed
                     -- Cost is hard-coded to 100 for now, this will be fixed later
                     playerMoving 100 pendingWorld w
                  )

      -- Get the updated world
      w2 <- askWorld

      -- Handle the annotations
      -- This is not terribly pretty as its doing a select for update, but its good enough for debugging
      -- the annotation code can be removed once everything is working
      let annotations = w2 ^. wdUtilBrainAnnotations 
      modifyWorld (\w -> w & wdUtilBrainAnnotations .~ [])
      printAnnotations annotations

      -- Draw
      w3 <- askWorld
      sendHostData $ Ae.encodeText (drawAndSend w3)

    _ ->
      sendHostError $ "Unknown command: " <> cmd

  where
    updatePlayer f = modifyWorld (\w -> w & wdPlayer %~ f)

    printAnnotations as = do
      debugPrint ""
      debugPrint ""
      debugPrint ""
      debugPrint ""
      debugPrint "***** Utility Annotations **************"
      traverse_ printAnnotation as
      debugPrint "****************************************"
      debugPrint ""

    printAnnotation (e, assess, top)  = do
      debugPrint ""
      debugPrint $ "-----------------------" <> show e
      debugPrint "  -- assess --"
      debugPrint . Txt.intercalate "\n" $ showEntries <$> assess
      debugPrint ""
      debugPrint "  -- top --"
      debugPrint . Txt.intercalate "\n" $ showEntries <$> top
      debugPrint "-----------------------"

    showEntries :: UtilAnnotationEntry -> Text
    showEntries e =
      case e of
        UeAt a -> "    At: " <> a
        UeSelectTopNone n -> "    No utils: " <> n
        UeSelectTopAbove f  -> "    Top above: " <> showF f
        UeSelectTopOne val n i d -> "    Select top one: " <> n <> ", impulse=" <> show i <> ", score=" <> showF val <> "," <> d
        UeNote n -> "    Note: " <> n
----------------------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------------------
-- L3 (Business logic, pure code only)
----------------------------------------------------------------------------------------------------------------
parseCommand :: Text -> Maybe (Text, [Text])
parseCommand t =
  case Txt.splitOn "|" t of
    (c:d) -> Just (c, d)
    _ -> Nothing


initialiseConnection :: Host.Connection -> [Text] -> Rnd.StdGen -> (Levels -> Level) -> Either Text World
initialiseConnection conn cmdData std getLevel = 
  case parseScreenSize cmdData of
    Nothing ->
      Left "missing / invalid screen size"

    Just (width, height) ->
      Right $ bootWorld conn (width, height) std getLevel Levels01

  
bootWorld :: Host.Connection -> (Int, Int) -> Rnd.StdGen -> (Levels -> Level) -> Levels -> World
bootWorld conn screenSize std getLevel startLevel = 
  let
    config = mkConfig
    level = getLevel startLevel

    w1 = World { _wdPlayer = mkPlayer
               , _wdConfig = config
               , _wdMap = loadWorld E.loadTexts $ level ^. lvlMapText
               , _wdActors = Map.fromList []
               , _wdMinMoveEnergy = 100
               , _wdEnergyIncrements = 20
               , _wdUtilBrainAnnotations = []
               , _wdGetLevel = getLevel
               , _wdLevel = level
               }

    w2 = level ^. lvlBoot $ w1
  in
  -- Calculate the actors fov
  updateAllActors w2 updateActorFov

  where
    mkConfig =
      Config { _cfgKeys = Map.fromList [ ("up"      , "Move:up")
                                       , ("k"       , "Move:up")
                                       , ("down"    , "Move:down")
                                       , ("j"       , "Move:down")
                                       , ("left"    , "Move:left")
                                       , ("h"       , "Move:left")
                                       , ("right"   , "Move:right")
                                       , ("l"       , "Move:right")
                                       , ("u"       , "Move:up-right")
                                       , ("pageup"  , "Move:up-right")
                                       , ("y"       , "Move:up-left")
                                       , ("home"    , "Move:up-left")
                                       , ("n"       , "Move:down-right")
                                       , ("end"     , "Move:down-left")
                                       , ("b"       , "Move:down-left")
                                       , ("pagedown", "Move:down-right")

                                       , ("shift+v c", "Game:ViewPort:Centre")
                                       , ("shift+v s", "Game:ViewPort:Scroll")
                                       , ("shift+v p", "Game:ViewPort:Snap")
                                       , ("shift+v b", "Game:ViewPort:Border")
                                       , ("shift+v l", "Game:ViewPort:Lock")

                                       , ("shift+d d", "Debug:Light:Toggle")
                                       ]
             , _cfgMinMaxBounds = (-300, 300, -300, 300)
             }

    mkPlayer =
      Player { _plConn = conn
             , _plScreenSize = screenSize
             , _plWorldTopLeft = WorldPos (0, 0)
             , _plActor = mkPlayersActor
             , _plViewPortStyle = ViewPortBorder 2
             , _plPendingEnergy = 0
             }

    mkPlayersActor =
      Actor { _acId = Aid "player"
            , _acClass = ClassPlayer
            , _acEntity = E.getEntity E.Player
            , _acWorldPos = WorldPos (1, -1)
            , _acStdGen = std
            , _acFovDistance = 3
            , _acFov = Nothing
            , _acFovHistory = Set.empty
            , _acSkipMove = False
            , _acMoveEnergyCost = 100
            , _acEnergy = B.new 200 100
            , _acUtilities = []
            , _acDisposition = UB.emptyDisposition
            , _acPosMemory = M.empty
            , _acProps = Map.empty
            }
    

buildConfig :: Config -> UiConfigData
buildConfig cfg =
  UiConfigData { udKeys = buildKeys (cfg ^. cfgKeys)
               , udBlankId = E.getTile E.Blank ^. tlId
               }

  where
    buildKeys ks = buildKey <$> Map.toList ks
    buildKey (s, a) = UiKey s a


parseScreenSize :: [Text] -> Maybe (Int, Int)
parseScreenSize cmd = do
  (tx, ty) <- case cmd of
                (tx : ty : _) -> Just (tx, ty)
                _ -> Nothing

  x <- (readMaybe . Txt.unpack $ tx) :: Maybe Int
  y <- (readMaybe . Txt.unpack $ ty) :: Maybe Int
  pure (x, y)


drawAndSend :: World -> UiDrawCommand
drawAndSend world = 
  let layers = drawTilesForPlayer world (world ^. wdMap) in
  
  UiDrawCommand { drCmd = "draw"
                , drScreenWidth = world ^. wdPlayer ^. plScreenSize ^. _1
                , drMapData = mkDrawMapData <<$>> (Map.toList <$> layers)
                }

  where
    mkDrawMapData :: (PlayerPos, Tile) -> (Int, Int, Int)
    mkDrawMapData (PlayerPos (x, y), tile) = (x, y, tile ^. tlId)

  
loadWorld :: Map Text Entity -> Text -> Map WorldPos Entity
loadWorld chars csv = 
  translatePlayerMap (WorldPos (0, 0)) $ parseWorld chars csv


parseWorld :: Map Text Entity -> Text -> Map PlayerPos Entity
parseWorld chars csv = 
  let ls = Txt.lines csv in
  let lss = Txt.strip <<$>> (Txt.splitOn "," <$> ls) in
  let entityMap = Lst.imap (\r cs -> Lst.imap (loadCol r) cs) lss in
  Map.fromList . catMaybes $ concat entityMap

  where
    loadCol y x c = case Map.lookup c chars of
                      Nothing -> Nothing
                      Just a -> Just (PlayerPos (x, y), a)


translatePlayerMap :: WorldPos -> Map PlayerPos Entity -> Map WorldPos Entity
translatePlayerMap worldTopLeft entityMap =
  let entitysInWorld = Ar.first (playerCoordToWorld worldTopLeft) <$> Map.toList entityMap  in
  Map.fromList entitysInWorld


playerCoordToWorld :: WorldPos -> PlayerPos -> WorldPos
playerCoordToWorld (WorldPos (worldTopX, worldTopY)) (PlayerPos (playerX, playerY)) =
   WorldPos (worldTopX + playerX, worldTopY - playerY)


worldCoordToPlayer :: WorldPos -> WorldPos -> PlayerPos
worldCoordToPlayer (WorldPos (worldTopX, worldTopY)) (WorldPos (worldX, worldY)) =
   PlayerPos (worldX - worldTopX, -(worldY - worldTopY))

  
drawTilesForPlayer :: World -> Map WorldPos Entity -> [Map PlayerPos Tile]
drawTilesForPlayer world entityMap =
  let
    -- Entity base layer
    entities = mkLayer entityMap
    -- Darkness
    darknessOverlay = darknessFovOverlay (world ^. wdPlayer) (world ^. wdPlayer ^. plActor)
    -- Darkness hides entity
    baseLayer = Map.union darknessOverlay entities

    -- Actor layer on top
    actorMap = Map.fromList $ (\a -> (a ^. acWorldPos, a ^. acEntity)) <$> getAllActors world
    inViewActors = Map.filterWithKey inView actorMap
    actorLayer = mkLayer inViewActors
    visibleActorLayer = Map.filterWithKey (\wp _ -> isNotDarkness wp baseLayer) actorLayer
  in
    -- Layers
    -- 0: Entities (with darkness overlay)
    -- 1: Actors
    [baseLayer, visibleActorLayer]

  where
    player = world ^. wdPlayer
    
    -- Top left of player's grid
    (WorldPos (topX, topY)) = player ^. plWorldTopLeft 

    -- Players screen/grid dimensions
    (screenX, screenY) = player ^. plScreenSize 

    -- Bottom right corner
    (bottomX, bottomY) = (topX + screenX, topY - screenY) 

    isNotDarkness :: PlayerPos -> Map PlayerPos Tile -> Bool
    isNotDarkness wp ts =
      case Map.lookup wp ts of
        Nothing -> True
        Just t -> t ^. tlId /= E.getTile E.Dark ^. tlId
  
    inView (WorldPos (x, y)) _ =
      x >= topX && x < bottomX && y > bottomY && y <= topY

    mkLayer :: Map WorldPos Entity -> Map PlayerPos Tile
    mkLayer entities =
      let
        -- Filter out blank
        noEmptyMap = Map.filter (\e -> e ^. enTile ^. tlName /= "blank") entities 

        -- Only get the entitys that are at positions on the player's screen
        visibleEntitys = Map.filterWithKey inView noEmptyMap

        -- Get the tile for each entity
        tileMap = (^. enTile) <$> visibleEntitys 
      in
      -- Get it with player positions
      Map.mapKeys (worldCoordToPlayer $ player ^. plWorldTopLeft) tileMap


getAllActors :: World -> [Actor]
getAllActors world =
  world ^. wdPlayer ^. plActor : Map.elems (world ^. wdActors)


handleKey :: World -> [Text] -> [RogueAction]
handleKey world (cmd:_) = 
  let actor = world ^. wdPlayer ^. plActor in
  let topLeft = world ^. wdPlayer ^. plWorldTopLeft in

  case cmd of
    "Move:up"         -> [ActMovePlayer ( 0,  1)]
    "Move:down"       -> [ActMovePlayer ( 0, -1)]
    "Move:left"       -> [ActMovePlayer (-1,  0)]
    "Move:right"      -> [ActMovePlayer ( 1,  0)]
    "Move:up-right"   -> [ActMovePlayer ( 1,  1)]
    "Move:up-left"    -> [ActMovePlayer (-1,  1)]
    "Move:down-right" -> [ActMovePlayer ( 1, -1)]
    "Move:down-left"  -> [ActMovePlayer (-1, -1)]

    "Game:ViewPort:Centre" -> [ActSetPlayerViewPortStyle ViewPortCentre]
    "Game:ViewPort:Scroll" -> [ActSetPlayerViewPortStyle ViewPortScroll]
    "Game:ViewPort:Snap"   -> [ActSetPlayerViewPortStyle ViewPortSnapCentre]
    "Game:ViewPort:Border" -> [ActSetPlayerViewPortStyle $ ViewPortBorder 2]
    "Game:ViewPort:Lock"   -> [ActSetPlayerViewPortStyle $ ViewPortLock (worldCoordToPlayer topLeft $ actor ^. acWorldPos)]

    "Debug:Light:Toggle" -> [ActTogglePlayerProp "debug:light" "on"]

    _ -> []
handleKey _ _ = []


runActions :: World -> [RogueAction] -> World
runActions world actions =
  foldl' runAction world actions


runAction :: World -> RogueAction -> World
runAction world action =
  case action of
    ActMovePlayer move  ->
      fromMaybe world $ tryMoveActor world (world ^. wdPlayer ^. plActor) move

    ActSetPlayerViewPortStyle style ->
      world & (wdPlayer . plViewPortStyle) .~ style

    ActTogglePlayerProp prop valEnabled ->
      world & (wdPlayer . plActor . acProps) %~ Map.alter (toggleMapProp valEnabled) prop

    ActMoveActor actor worldPos ->
      let
        movedActor = actor & acWorldPos .~ worldPos
        w2 = updatePlayerViewport $ updateActor world movedActor
        pa = w2 ^. wdPlayer ^. plActor
      in
        updateActor w2 (updateActorFov w2 pa)

    ActGotoLevel l ->
      bootWorld
        (world ^. wdPlayer ^. plConn)
        (world ^. wdPlayer ^. plScreenSize)
        (world ^. wdPlayer ^. plActor ^. acStdGen)
        (world ^. wdGetLevel)
        l

  where
    toggleMapProp v Nothing = Just v
    toggleMapProp _ (Just _) = Nothing


tryMoveActor :: World -> Actor -> (Int, Int) -> Maybe World
tryMoveActor world actor (dx, dy) =
  let
    -- Get the world bounds
    (minX, maxX, minY, maxY) = world ^. wdConfig ^. cfgMinMaxBounds

    -- Actor's position
    (WorldPos wdPos) = actor ^. acWorldPos 

    -- Where the actor whats to move to, using bimap :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
    tryWorldTo@(tx, ty) = bimap (+ dx) (+ dy) wdPos 
    tryWorldTo' = WorldPos tryWorldTo 
  in

  -- Is the actor trying to move out of the world?
  if tx < minX || ty < minY || tx >= maxX || ty >= maxY
  then Nothing
  else
    let
      -- Entity at destination
      destEntity = (world ^. wdMap ^.at tryWorldTo') 
      destEntityType = _enType <$> destEntity
      -- Actors at destination
      destActors = filter (\a -> a ^. acWorldPos == tryWorldTo') (getAllActors world)
      -- Get actions
      actions = (world ^. wdLevel ^. lvlTryMove) destActors destEntityType world tryWorldTo' actor
   in
   Just $ runActions world actions 


updateActorFov :: World -> Actor -> Actor
updateActorFov w a =
  -- Calculate field of view
  let fov = calcFov (a ^. acFovDistance) (isTransparent $ w ^. wdMap) (a ^. acWorldPos) in
  a & acFov .~ Just fov
    & acFovHistory %~ Set.union (Set.fromList $ flatFov (Just fov))



-- | Update either the player's actor, or one of the world actors
updateActor :: World -> Actor -> World
updateActor w actor =
  if w ^. wdPlayer ^. plActor ^. acId == (actor ^. acId)
  then w & (wdPlayer . plActor) .~ actor                         -- update the player's actor
  else w & wdActors %~ Map.adjust (const actor) (actor ^. acId)  -- update other actor, nop if aid not found

  
-- | Update either the player's actor, or one of the world actors
updateActorById :: World -> Aid -> (Actor -> Actor) -> World
updateActorById w id update =
  if w ^. wdPlayer ^. plActor ^. acId == id
  then w & (wdPlayer . plActor) .~ update (w ^. wdPlayer ^. plActor) -- update the player's actor
  else w & wdActors %~ Map.adjust update id                          -- update other actor, nop if aid not found

  
-- | Update all actors, including the player's actor
updateAllActors :: World -> (World -> Actor -> Actor) -> World
updateAllActors w fn =
  let w2 = w & (wdPlayer . plActor) %~ fn w in
  let w3 = w2 & wdActors %~ fmap (fn w2) in
  w3


-- | Update the player's view port
updatePlayerViewport :: World -> World
updatePlayerViewport w =
  let p = w ^. wdPlayer in
  w & wdPlayer .~ (p & plWorldTopLeft .~ calcViewPortTopLeft p)


calcViewPortTopLeft :: Player -> WorldPos
calcViewPortTopLeft player =
  let actor = player ^. plActor in

  case player ^. plViewPortStyle of
    -- These two styles put the player in the viewport, so no need to check
    ViewPortCentre -> centreOn (player ^. plScreenSize) (actor ^. acWorldPos)
    ViewPortLock focus -> focusOn focus $ actor ^. acWorldPos

    _ -> 
      let tl@(WorldPos (tX, tY)) = (player ^. plWorldTopLeft) in
      let sz@(width, height) = (player ^. plScreenSize) in
      let (outX, outY) = distanceOutOfViewPort sz tl (actor ^. acWorldPos) in
      
      case player ^. plViewPortStyle of
        ViewPortSnapCentre ->
          if outX /= 0 || outY /= 0
          then centreOn (player ^. plScreenSize) (actor ^. acWorldPos)
          else player ^. plWorldTopLeft

        ViewPortBorder d ->
          let (outX', outY') = distanceOutOfViewPort
                                 (width - d - d, height - d - d)
                                 (WorldPos (tX + d, tY - d))
                                 (actor ^. acWorldPos)
          in
          WorldPos (tX + outX', tY + outY')

        _ -> -- default to ViewPortScroll
          WorldPos (tX + outX, tY + outY)

  where
    centreOn :: (Int, Int) -> WorldPos -> WorldPos
    centreOn (screenWidth, screenHeight) (WorldPos (wAtX, wAtY)) =
      let (sMidX, sMidY) = (screenWidth `div` 2, screenHeight `div` 2) in
      WorldPos (wAtX - sMidX, wAtY + sMidY)
    
    
    focusOn :: PlayerPos -> WorldPos -> WorldPos
    focusOn (PlayerPos (focusX, focusY)) (WorldPos (atX, atY)) =
      WorldPos (atX - focusX, atY + focusY)
      
    
    distanceOutOfViewPort :: (Int, Int) -> WorldPos -> WorldPos -> (Int, Int)
    distanceOutOfViewPort (screenWidth, screenHeight) (WorldPos (topX, topY)) (WorldPos (atX, atY)) =
      let
        x = if | atX <  topX               -> atX - topX
               | atX >= topX + screenWidth -> atX - topX - screenWidth + 1
               | otherwise -> 0

        y = if | atY >  topY                -> atY - topY
               | atY <= topY - screenHeight -> atY - (topY - screenHeight + 1)
               | otherwise -> 0
      in
      (x, y)


-- | Calculate the field of view from a position
calcFov :: Int -> (WorldPos -> Bool) -> WorldPos -> [(WorldPos, [WorldPos])]
calcFov fovDistance isEntityTransparent fromPos'@(WorldPos fromPos) =
  let boundries = getBoundries fromPos' in
  go <$> boundries

  where
    getBoundries (WorldPos (x, y)) = boundingPoints fovDistance (WorldPos (x, y))

    go toPos'@(WorldPos toPos) =
      let line = WorldPos <$> bline fromPos toPos in
      let isTransparentOrStart p = p == fromPos' || isEntityTransparent p in
      let (m, r) = Lst.span isTransparentOrStart line in
      (toPos', m <> Lst.take 1 r)


-- | Get the bounds for a fov distance
boundingPoints :: Int -> WorldPos -> [WorldPos]
boundingPoints distance (WorldPos (atx, aty)) =
  Lst.nub $
    [WorldPos (atx - distance + d, aty - distance) | d <- [0..distance * 2]] <>
    [WorldPos (atx - distance, aty - distance + d) | d <- [0..distance * 2]] <>
    [WorldPos (atx - distance + d, aty + distance) | d <- [0..distance * 2]] <>
    [WorldPos (atx + distance, aty - distance + d) | d <- [0..distance * 2]]
              

-- | Bresenham's algorithm
-- | https://wiki.haskell.org/Bresenham%27s_line_drawing_algorithm
bline :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
bline pa@(xa, ya) pb@(xb, yb) =
  let r = map maySwitch . Lst.unfoldr go $ (x1, y1, 0) in

  case r of
    (p:_) | p == pa -> r
    _ -> Lst.reverse r

  where
    steep = abs (yb - ya) > abs (xb - xa)
    maySwitch = if steep then (\(x,y) -> (y,x)) else identity
    ((x1, y1), (x2, y2)) = case Lst.sort [maySwitch pa, maySwitch pb] of
                             [a, b] -> (a, b)
                             _ -> ((0, 0), (0, 0)) -- This case is never matched, but fixes partial match warning
    deltax = x2 - x1
    deltay = abs (y2 - y1)
    ystep = if y1 < y2 then 1 else -1
    go (xTemp, yTemp, err)
        | xTemp > x2 = Nothing
        | otherwise = Just ((xTemp, yTemp), (xTemp + 1, newY, newError))
        where
          tempError = err + deltay
          (newY, newError) = if (2 * tempError) >= deltax
                            then (yTemp + ystep, tempError - deltax)
                            else (yTemp, tempError)

  
isTransparent :: Map WorldPos Entity -> WorldPos -> Bool
isTransparent wmap pos =
  case Map.lookup pos wmap of
    Nothing -> True
    Just e -> (e ^. enType) /= E.Wall

  
darknessFovOverlay :: Player -> Actor -> Map PlayerPos Tile
darknessFovOverlay player actor =
  case player ^. plActor ^. acProps ^.at "debug:light" of
    Nothing ->
      let
        (screenWidth, screenHeight) = player ^. plScreenSize

        -- Create a full grid of darkness
        blackBg = Map.fromList [ (PlayerPos (x, y), E.getTile E.Dark)
                               | x <- [0..(screenWidth - 1)]
                               , y <- [0..(screenHeight - 1)]
                               ] 

        lightAt = worldCoordToPlayer (player ^. plWorldTopLeft) <$> flatFov (actor ^. acFov)
        seen = worldCoordToPlayer (player ^. plWorldTopLeft) <$> Set.toList (actor ^. acFovHistory)
      in
      -- Remove the darkness overlay at any position that is to be lit
      --  I.e. any position in the field of view, or previously in the field of view
      foldr Map.delete blackBg $ lightAt <> seen

    Just _ ->
      Map.empty

  
flatFov :: Maybe [(WorldPos, [WorldPos])] -> [WorldPos]
flatFov Nothing = []
flatFov (Just fov) = Lst.nub . Lst.concat $ snd <$> fov

  
-- | Manages the core logic of the energy system.
--    
--       [key press] ------> is zero cost move?
--                                |
--                                |
--              +--<----yes-------+-->--no-----+
--              |                              |
--              v                              |
--        +-->(exit)                           |
--        |     ^                              |
--        |     |                              v
--        |     +--<----no--------player has min move energy?
--        |                          |         
--        |                         yes
--        |                          |
--        |                          v
--        |                      move player
--        |                          |         
--        |                          v         
--        +--<---yes----player still has > min move energy
--                         and is not skipping a move?
--                                   |         
--                                  no
--                                   |
--                                   v
--                ###################################################
--                #                  |                              #
--                #                  v                              #
--          +--<--------player has > min move energy <--------+     #
--          |     #                  |                        |     #
--         yes    #                 no                        |     #
--          |     #                  |                        |     #
--          |     #                  v                        |     #
--          |     #    move every non-player actor that       |     #
--          |     #     has > min move energy and has         |     #
--          |     #     not elected to skip a move.           |     #
--          |     #                  |                        |     #
--          |     #                  |                        |     #
--          |     #                  v                        |     #
--          |     #     add wdEnergyIncrements to all actors--+     #
--          |     #           including player's actor              #
--          |     #                                                 #
--          |     ###################################################
--          |
--          |
--          +---------------> set all actors skipMove = False
--                                       |
--                                       |
--                                       v
--                                     (exit)
--  
--  
playerMoving :: Int -> World -> World -> World
playerMoving pendingCost pendingWorld oldWorld = 
  let playerAttemptedMoveWorld = 
        Right oldWorld
          >>= checkIfNonMove
          >>= checkIfPlayerHasMinEnergy
          >>= runPendingIfPlayerHasEnergy
          >>= runPlayerTick -- run the tick for the player, this is only run if the move was allowed
          >>= stopIfPlayerCanStillMove
  in
  case playerAttemptedMoveWorld of
    Left w -> w -- Left means stop 
    Right w ->  -- Right means continue with other actors
      -- Loop, adding energy (wdEnergyincrements) to all actors until the player has enough energy to move
      storeSkipTurnEnergy w
      & runNonPlayerActorLoop
      & restoreSkipTurnEnergy
      & disableSkip
  
  where
    checkIfNonMove w =
      -- If the cost is zero/negative then this is not an actual move
      --  Apply the pending action and continue
      if pendingCost <= 0 && not (pendingWorld ^. wdPlayer ^. plActor ^. acSkipMove)
      then Left pendingWorld
      else Right w

    checkIfPlayerHasMinEnergy w =
      if B.get (w ^. wdPlayer ^. plActor ^. acEnergy) >= w ^. wdMinMoveEnergy
      then Right w -- continue
      else Left w  -- not enough energy to move regardless of move cost
    
    runPendingIfPlayerHasEnergy w =
      if B.get (w ^. wdPlayer ^. plActor ^. acEnergy) >= pendingCost
      then
        -- perform move and subtract energy
        Right (pendingWorld & (wdPlayer . plActor . acEnergy) %~ B.update (subtract pendingCost))
      else
        -- disallow
        Left w

    runPlayerTick w =
      Right $ w & (wdPlayer . plActor) %~ actorTick

    stopIfPlayerCanStillMove w =
      let
        a = w ^. wdPlayer ^. plActor 
        hasEnergy = B.get (a ^. acEnergy) > a ^. acMoveEnergyCost 
        skipMove = a ^. acSkipMove 
      in
      if
        | skipMove -> Right w -- The player elected to skip a move, continue with others
        | hasEnergy -> Left w -- The player has energy, its still their turn
        | otherwise -> Right w -- continue

    runNonPlayerActorLoop w =
      if B.get (w ^. wdPlayer ^. plActor ^. acEnergy) >= w ^. wdMinMoveEnergy
      then
        w -- The player now has enough energy to move, stop loop
      else
        let
          -- Move actors
          w' = moveAllNonPlayers w 
          -- Add energy for next loop
          addEnergy _ a = a & acEnergy %~ B.update ((w' ^. wdEnergyIncrements) +)
        in
        runNonPlayerActorLoop $ updateAllActors w' addEnergy

    moveAllNonPlayers w =
      let mv aOrig wOrig =
            let
              inFov = findPathToAllInFov wOrig aOrig 
              ((utilities, wNext), annAssess) = runWriter $ UB.assessUtilities inFov wOrig aOrig 
              (topUtil, annTop) = runWriter $ UB.selectTopUtility utilities
              annotation = (aOrig ^. acEntity ^. enType, DLst.toList annAssess, DLst.toList annTop)
              addAnn w' = w' & wdUtilBrainAnnotations %~ (annotation :)
            in

            case topUtil of
              Nothing ->
                -- No utility = no move, skip
                updateActorById (addAnn wNext) (aOrig ^. acId) (\a -> a & acSkipMove .~ True)

              Just (_, actorIfMoved, action, _, _) ->
                let cost = floor . fromIntegral $ aOrig ^. acMoveEnergyCost in
                
                if cost > B.get (aOrig ^. acEnergy)
                then
                  -- Not enough energy to move, disallow. Set skipMove = True so this is not attempted again before
                  -- the next actor move (i.e. avoid looping)
                  wNext & wdActors %~ Map.insert (aOrig ^. acId) (aOrig & acSkipMove .~ True)
                else
                  actOnImpulse cost (addAnn wNext) actorIfMoved action
      in

      let actorsThatCanMove = filter
                                (\a -> B.get (a ^. acEnergy) >= (w ^. wdMinMoveEnergy) && not (a ^. acSkipMove))
                                (Map.elems $ w ^. wdActors)
      in
      -- Are the any actors that could still move?
      if null actorsThatCanMove
      then
        w -- No one left, done
      else
        -- Give actors that are able to move a chance to move
        foldr mv w actorsThatCanMove
      
      
    storeSkipTurnEnergy w =
      if w ^. wdPlayer ^. plActor ^. acSkipMove
      then
        -- Store the player's current energy, and set the energy level to zero
        -- This lets the actor movement loop run for a full set of turns up to the min energy level
        w & (wdPlayer . plPendingEnergy) .~ B.get (w ^. wdPlayer ^. plActor ^. acEnergy)
          & (wdPlayer . plActor . acEnergy) %~ B.set 0
      else
        w
      
    restoreSkipTurnEnergy w =
      if w ^. wdPlayer ^. plActor ^. acSkipMove
      then
        -- Restore and pending energy, up to the player's max energy level
        w & (wdPlayer . plActor . acEnergy) %~ B.update ((w ^. wdPlayer ^. plPendingEnergy) +)
      else
        w
      
    disableSkip w =
      updateAllActors w (\_ a -> a & acSkipMove .~ False)

  
actOnImpulse :: Int -> World -> Actor -> Impulse -> World
actOnImpulse cost w actorIfMoved impulse =
  let (dx, dy, nextStdGen) =
        let initialStdGen = (actorIfMoved ^. acStdGen) in

        case impulse of
          ImpMoveRandom ->
            let
              (dx', s1) = Rnd.randomR (-1, 1) initialStdGen
              (dy', s2) = Rnd.randomR (-1, 1) s1 
            in
            (dx', dy', s2)

          ImpMoveTowards (Path ps) ->
            case ps of
              (_:WorldPos (tx, ty):_) ->
                let (WorldPos (fx, fy)) = actorIfMoved ^. acWorldPos in
                (tx - fx, ty - fy, initialStdGen)
              _ -> (0, 0, initialStdGen)

  in
  if dx /=0 || dy /= 0
  then
    let worldIfMoved = w & wdActors %~ Map.insert (actorIfMoved ^. acId) actorIfMoved in
     
    case tryMoveActor worldIfMoved actorIfMoved (dx, dy) of
      Nothing ->
        w & wdActors %~ Map.adjust (\a' -> a' & acStdGen .~ nextStdGen) (actorIfMoved ^. acId)

      Just w' ->
        w' & wdActors %~ Map.adjust (\a' -> updateActorFov w' $ a' & acEnergy %~ B.update (subtract cost)
                                                                   & acStdGen .~ nextStdGen
                                    )
                                    (actorIfMoved ^. acId)
  else
    w & wdActors %~ Map.adjust (\a' -> a' & acStdGen .~ nextStdGen) (actorIfMoved ^. acId)


randomElement :: Rnd.StdGen -> [a] -> (Maybe a, Rnd.StdGen)
randomElement g as =
  let (i, next) = Rnd.randomR (0, length as - 1) g in
  (atMay as i, next)

  
findPathToAllInFov :: World -> Actor -> [PathTo]
findPathToAllInFov w a =
  case a ^. acFov of
    Nothing -> []
    Just fov ->
      let wmap = addActorsToMap w in
      concat (findPaths wmap <$> fov)

  where
    findPaths :: Map WorldPos Entity -> (WorldPos, [WorldPos]) -> [PathTo]
    findPaths wmap (dest, points) =
      snd $ foldl'
              (\(trail, paths) atPos -> (trail <> [atPos], paths <> findAt dest wmap (trail <> [atPos]) atPos))
              ([], [])
              points
      

    findAt :: WorldPos -> Map WorldPos Entity -> [WorldPos] -> WorldPos -> [PathTo]
    findAt dest wmap trail atPos =
      let ps = if atPos == w ^. wdPlayer ^. plActor ^. acWorldPos
               then [ PathToPlayer (Path trail) (w ^. wdPlayer) dest
                    , PathToActor (Path trail) (w ^. wdPlayer ^. plActor) dest
                    ]
               else []
      in
      let es = case wmap ^.at atPos of
                 Nothing -> []
                 Just e -> if e ^. enType == E.Blank
                              then []
                              else [PathToEntity (Path trail) e dest]
      in
      ps <> es

  
addActorsToMap :: World -> Map WorldPos Entity
addActorsToMap w =
  foldr
    (\a g -> Map.insert (a ^. acWorldPos) (a ^. acEntity) g)
    (w ^. wdMap)
    (getAllActors w)

  
actorTick :: Actor -> Actor
actorTick a =
  a & acPosMemory %~ M.tick
