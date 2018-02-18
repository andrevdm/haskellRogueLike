{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module GameEngine where

import Protolude hiding (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Txt
import qualified Data.Text.Encoding as TxtE
import qualified Data.Aeson.Text.Extended as Ae
import qualified Data.ByteString.Lazy as BSL
import qualified Codec.Compression.BZip as Bz
import           Control.Lens (_1, (^.), (.~), (%~))
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
    Just ("init", cmdData) ->
      case initialiseConnection conn cmdData of
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
      

initialiseConnection :: Host.Connection -> [Text] -> Either Text World
initialiseConnection conn cmdData = 
  case parseScreenSize cmdData of
    Nothing ->
      Left "missing / invalid screen size"

    Just (width, height) ->
      Right $ bootWorld conn (width, height) 


bootWorld :: Host.Connection -> (Int, Int) -> World
bootWorld conn screenSize = 
  World { _wdPlayer = mkPlayer
        , _wdConfig = mkConfig
        }
  where
    mkConfig =
      Config { _cfgKeys = Map.fromList [("t", "test")] }

    mkPlayer =
      Player conn screenSize
    

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
      
    "key" ->
      sendLog conn $ "TODO: " <> cmd <> ": " <> show cmdData

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
  let cmd = Ae.encodeText UiDrawCommand { drCmd = "draw"
                                        , drScreenWidth = world ^. wdPlayer ^. plScreenSize ^. _1
                                        }
  sendData (world ^. wdPlayer ^. plConn) cmd
