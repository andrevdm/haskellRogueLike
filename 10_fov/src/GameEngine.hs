{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module GameEngine where

import Protolude hiding (Map)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.List as Lst
import qualified Data.List.Index as Lst
import qualified Data.Text as Txt
import qualified Data.Text.IO as Txt
import qualified Data.Text.Encoding as TxtE
import qualified Data.Aeson.Text.Extended as Ae
import qualified Data.ByteString.Lazy as BSL
import qualified Codec.Compression.BZip as Bz
import qualified System.Random as Rnd
import           Control.Lens (at, _1, (^.), (.~), (%~))
import qualified Control.Arrow as Ar
import           Control.Concurrent.STM (atomically, readTVar, newTVar, modifyTVar', TVar)

import           GameCore
import qualified GameHost as Host
import           GameHost (conSendData, conReceiveText)
import qualified Entities as E
import qualified EntityType as E


runGame :: IO ()
runGame = Host.runHost manageConnection

      
manageConnection :: Host.Connection -> IO ()
manageConnection conn = do
  initCmd <- conn ^. conReceiveText 

  case parseCommand initCmd of
    Just ("init", cmdData) -> do
      mapData <- Txt.readFile "worlds/simple.csv"
      std <- Rnd.getStdGen
      
      case initialiseConnection conn cmdData mapData std of
        Right world -> do
          worldV <- atomically $ newTVar world
          sendConfig conn $ world ^. wdConfig
          runConnection worldV
        Left e ->
          sendError conn e
        
    _ ->
      pass

  where
    runConnection worldV = 
      forever $ do
        t <- conn ^. conReceiveText

        case parseCommand t of
          Nothing -> putText $ "error parsing: " <> t
          Just (cmd, cmdData) -> runCmd conn worldV cmd cmdData

    parseCommand :: Text -> Maybe (Text, [Text])
    parseCommand t =
      case Txt.splitOn "|" t of
        (c:d) -> Just (c, d)
        _ -> Nothing
      

initialiseConnection :: Host.Connection -> [Text] -> Text -> Rnd.StdGen -> Either Text World
initialiseConnection conn cmdData mapData std = 
  case parseScreenSize cmdData of
    Nothing ->
      Left "missing / invalid screen size"

    Just (width, height) ->
      Right $ bootWorld conn (width, height) mapData std


{-! SECTION< 10_bootWorld !-}
bootWorld :: Host.Connection -> (Int, Int) -> Text -> Rnd.StdGen -> World
bootWorld conn screenSize mapData std = 
  let
    config = mkConfig
    bug = mkEnemyActor "bug1" E.Bug (6, -2)
    snake = mkEnemyActor "snake1" E.Snake (8, -4)

    w1 = World { _wdPlayer = mkPlayer
               , _wdConfig = config
               , _wdMap = loadWorld E.loadTexts mapData
               , _wdActors = Map.fromList [ (bug ^. acId, bug)
                                          , (snake ^. acId, snake)
                                          ]
               }
  in

  -- Calculate the actors fov
  updateAllActors w1 updateActorFov
{-! SECTION> 10_bootWorld !-}

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
                                       ]
             , _cfgMinMaxBounds = (-300, 300, -300, 300)
             }

    mkPlayer =
      Player { _plConn = conn
             , _plScreenSize = screenSize
             , _plWorldTopLeft = WorldPos (0, 0)
             , _plActor = mkPlayersActor
             , _plViewPortStyle = ViewPortBorder 2
             }

    mkPlayersActor =
      Actor { _acId = Aid "player"
            , _acClass = ClassPlayer
            , _acEntity = E.getEntity E.Player
            , _acWorldPos = WorldPos (1, -1)
            , _acStdGen = std
            , _acFovDistance = 3
            , _acFov = Nothing
            }

    mkEnemyActor aid e (x, y) =
      Actor { _acId = Aid aid
            , _acClass = ClassEnemy
            , _acEntity = E.getEntity e
            , _acWorldPos = WorldPos (x, y)
            , _acStdGen = std
            , _acFovDistance = 2
            , _acFov = Nothing
            }
    

runCmd :: Host.Connection -> TVar World -> Text -> [Text] -> IO ()
runCmd conn worldV cmd cmdData = 
  case cmd of
    "redraw" -> 
      case parseScreenSize cmdData of
        Nothing -> sendError conn "missing / invalid screen size"
        Just (sx, sy) -> do
          updatePlayer (plScreenSize .~ (sx, sy))
          w <- atomically $ readTVar worldV
          drawAndSend w
          sendLog conn "draw"
      
    "key" -> do
      -- Handle the key press
      atomically $ modifyTVar' worldV (\w -> runActions w $ handleKey w cmdData)
      -- Get the updated world
      w2 <- atomically $ readTVar worldV
      -- Draw
      drawAndSend w2

    _ ->
      sendError conn $ "Unknown command: " <> cmd

  where
    updatePlayer f = atomically $ modifyTVar' worldV (\w -> w & wdPlayer %~ f)

  
sendLog :: Host.Connection -> Text -> IO ()
sendLog conn err =
  sendData conn $ Ae.encodeText $ UiMessage "log" err


sendError :: Host.Connection -> Text -> IO ()
sendError conn err =
  sendData conn $ Ae.encodeText $ UiMessage "error" err


sendConfig :: Host.Connection -> Config -> IO ()
sendConfig conn config =
  sendData conn . Ae.encodeText $ UiConfig "config" (buildConfig config)


buildConfig :: Config -> UiConfigData
buildConfig cfg =
  UiConfigData { udKeys = buildKeys (cfg ^. cfgKeys)
               , udBlankId = E.getTile E.Blank ^. tlId
               }

  where
    buildKeys ks = buildKey <$> Map.toList ks
    buildKey (s, a) = UiKey s a


sendData :: Host.Connection -> Text -> IO ()
sendData conn t = do
  let lz = Bz.compress . BSL.fromStrict . TxtE.encodeUtf8 $ t
  conn ^. conSendData $ lz


parseScreenSize :: [Text] -> Maybe (Int, Int)
parseScreenSize cmd = do
  (tx, ty) <- case cmd of
                (tx : ty : _) -> Just (tx, ty)
                _ -> Nothing

  x <- (readMaybe . Txt.unpack $ tx) :: Maybe Int
  y <- (readMaybe . Txt.unpack $ ty) :: Maybe Int
  pure (x, y)


drawAndSend :: World -> IO ()
drawAndSend world = do
  let layers = drawTilesForPlayer world (world ^. wdMap) 
  
  let cmd = Ae.encodeText UiDrawCommand { drCmd = "draw"
                                        , drScreenWidth = world ^. wdPlayer ^. plScreenSize ^. _1
                                        , drMapData = mkDrawMapData <<$>> (Map.toList <$> layers)
                                        }
  sendData (world ^. wdPlayer ^. plConn) cmd

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

  
{-! SECTION< 10_drawTilesForPlayer !-}
drawTilesForPlayer :: World -> Map WorldPos Entity -> [Map PlayerPos Tile]
drawTilesForPlayer world entityMap =
  let
{-! SECTION< 10_baseLayer_drawTilesForPlayer !-}
    -- Entity base layer
    entities = mkLayer entityMap
    -- Darkness
    darknessOverlay = darknessFovOverlay (world ^. wdPlayer) (world ^. wdPlayer ^. plActor)
    -- Darkness hides entity
    baseLayer = Map.union darknessOverlay entities
{-! SECTION> 10_baseLayer_drawTilesForPlayer !-}

{-! SECTION< 10_actors_drawTilesForPlayer !-}
    -- Actor layer on top
    actorMap = Map.fromList $ (\a -> (a ^. acWorldPos, a ^. acEntity)) <$> getAllActors world
    inViewActors = Map.filterWithKey inView actorMap
    actorLayer = mkLayer inViewActors
    visibleActorLayer = Map.filterWithKey (\wp _ -> isNotDarkness wp baseLayer) actorLayer
{-! SECTION> 10_actors_drawTilesForPlayer !-}
{-! SECTION< 10_layers_drawTilesForPlayer !-}
  in
    -- Layers
    -- 0: Entities (with darkness overlay)
    -- 1: Actors
    [baseLayer, visibleActorLayer]
{-! SECTION> 10_layers_drawTilesForPlayer !-}

  where
    player = world ^. wdPlayer
    
    -- Top left of player's grid
    (WorldPos (topX, topY)) = player ^. plWorldTopLeft 

    -- Players screen/grid dimensions
    (screenX, screenY) = player ^. plScreenSize 

    -- Bottom right corner
    (bottomX, bottomY) = (topX + screenX, topY - screenY) 

{-! SECTION< 10_isNotDarkness_drawTilesForPlayer !-}
    isNotDarkness :: PlayerPos -> Map PlayerPos Tile -> Bool
    isNotDarkness wp ts =
      case Map.lookup wp ts of
        Nothing -> True
        Just t -> t ^. tlId /= E.getTile E.Dark ^. tlId
{-! SECTION> 10_isNotDarkness_drawTilesForPlayer !-}
  
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
{-! SECTION> 10_drawTilesForPlayer !-}


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
      -- Is the move allowed
      canMove = case (destActors, destEntityType) of
                  ([], Just E.Blank) -> True
                  ([], Just E.Door) -> True
                  ([], Nothing) -> True
                  _ -> False
      in
{-! SECTION< 10_tryMoveActor !-}
      if canMove
      then
        let
          movedActor = actor & acWorldPos .~ tryWorldTo'
          w2 = updatePlayerViewport $ updateActor world movedActor
          pa = w2 ^. wdPlayer ^. plActor
        in
          Just $ updateActor w2 (updateActorFov w2 pa)
      else
        Nothing
{-! SECTION> 10_tryMoveActor !-}

{-! SECTION< 10_updateActorFov !-}
updateActorFov :: World -> Actor -> Actor
updateActorFov w a =
  a & acFov .~ Just (calcFov (a ^. acFovDistance) (isTransparent $ w ^. wdMap) (a ^. acWorldPos))
{-! SECTION> 10_updateActorFov !-}


-- | Update either the player's actor, or one of the world actors
updateActor :: World -> Actor -> World
updateActor w actor =
  if w ^. wdPlayer ^. plActor ^. acId == (actor ^. acId)
  then w & (wdPlayer . plActor) .~ actor                         -- update the player's actor
  else w & wdActors %~ Map.adjust (const actor) (actor ^. acId)  -- update other actor, nop if aid not found

  
{-! SECTION< 10_updateAllActors !-}
-- | Update all actors, including the player's actor
updateAllActors :: World -> (World -> Actor -> Actor) -> World
updateAllActors w fn =
  let w2 = w & (wdPlayer . plActor) %~ fn w in
  let w3 = w2 & wdActors %~ fmap (fn w2) in
  w3
{-! SECTION> 10_updateAllActors !-}


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


{-! SECTION< 10_calcFov !-}
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
{-! SECTION> 10_calcFov !-}


{-! SECTION< 10_boundingPoints !-}
-- | Get the bounds for a fov distance
boundingPoints :: Int -> WorldPos -> [WorldPos]
boundingPoints distance (WorldPos (atx, aty)) =
  Lst.nub $
    [WorldPos (atx - distance + d, aty - distance) | d <- [0..distance * 2]] <>
    [WorldPos (atx - distance, aty - distance + d) | d <- [0..distance * 2]] <>
    [WorldPos (atx - distance + d, aty + distance) | d <- [0..distance * 2]] <>
    [WorldPos (atx + distance, aty - distance + d) | d <- [0..distance * 2]]
{-! SECTION> 10_boundingPoints !-}
              

{-! SECTION< 10_bline !-}
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
{-! SECTION> 10_bline !-}

  
{-! SECTION< 10_isTransparent !-}
isTransparent :: Map WorldPos Entity -> WorldPos -> Bool
isTransparent wmap pos =
  case Map.lookup pos wmap of
    Nothing -> True
    Just e -> (e ^. enType) /= E.Wall
{-! SECTION> 10_isTransparent !-}

  
{-! SECTION< 10_darknessFovOverlay !-}
darknessFovOverlay :: Player -> Actor -> Map PlayerPos Tile
darknessFovOverlay player actor =
  let
    (screenWidth, screenHeight) = player ^. plScreenSize

    -- Create a full grid of darkness
    blackBg = Map.fromList [ (PlayerPos (x, y), E.getTile E.Dark)
                           | x <- [0..(screenWidth - 1)]
                           , y <- [0..(screenHeight - 1)]
                           ] 

    lightAt = worldCoordToPlayer (player ^. plWorldTopLeft) <$> flatFov (actor ^. acFov)
  in
  -- Remove the darkness overlay at any position that is to be lit
  foldr Map.delete blackBg lightAt
{-! SECTION> 10_darknessFovOverlay !-}

  
{-! SECTION< 10_flatFov !-}
flatFov :: Maybe [(WorldPos, [WorldPos])] -> [WorldPos]
flatFov Nothing = []
flatFov (Just fov) = Lst.nub . Lst.concat $ snd <$> fov
{-! SECTION> 10_flatFov !-}
