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
import           Control.Lens ((^.), (.~), (%~))
import           Control.Concurrent.STM (atomically, newTVar, modifyTVar', TVar)

import           GameCore
import qualified GameHost as Host
import           GameHost (conSendData, conReceiveText)

{-! SECTION< 01_runGame !-}
runGame :: IO ()
runGame = Host.runHost manageConnection
{-! SECTION> 01_runGame !-}

      
{-! SECTION< 01_manageConnection !-}
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
{-! SECTION> 01_manageConnection !-}
      

{-! SECTION< 01_initialiseConnection !-}
initialiseConnection :: Host.Connection -> [Text] -> Either Text World
initialiseConnection conn cmdData = 
  case parseScreenSize cmdData of
    Nothing ->
      Left "missing / invalid screen size"

    Just (width, height) ->
      Right $ bootWorld conn (width, height) 
{-! SECTION> 01_initialiseConnection !-}


{-! SECTION< 01_bootWorld !-}
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
{-! SECTION> 01_bootWorld !-}
    

{-! SECTION< 01_runCmd !-}
runCmd :: Host.Connection -> TVar World -> Text -> [Text] -> IO ()
runCmd conn worldV cmd cmdData = 
  case cmd of
    "redraw" -> 
      case parseScreenSize cmdData of
        Nothing -> sendError conn "missing / invalid screen size"
        Just (sx, sy) -> do
          updatePlayer (plScreenSize .~ (sx, sy))
          sendLog conn $ "TODO: " <> cmd
      
    "key" ->
      sendLog conn $ "TODO: " <> cmd <> ": " <> show cmdData

    _ ->
      sendError conn $ "Unknown command: " <> cmd

  where
    updatePlayer f = atomically $ modifyTVar' worldV (\w -> w & wdPlayer %~ f)
{-! SECTION> 01_runCmd !-}

  
{-! SECTION< 01_other !-}
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
  UiConfigData $ buildKeys (cfg ^. cfgKeys)

  where
    buildKeys ks = buildKey <$> Map.toList ks
    buildKey (s, a) = UiKey s a


sendData :: Host.Connection -> Text -> IO ()
sendData conn t = do
  let lz = Bz.compress . BSL.fromStrict . TxtE.encodeUtf8 $ t
  conn ^. conSendData $ lz
{-! SECTION> 01_other !-}


{-! SECTION< 01_parseScreenSize !-}
parseScreenSize :: [Text] -> Maybe (Int, Int)
parseScreenSize cmd = do
  (tx, ty) <- case cmd of
                (tx : ty : _) -> Just (tx, ty)
                _ -> Nothing

  x <- (readMaybe . Txt.unpack $ tx) :: Maybe Int
  y <- (readMaybe . Txt.unpack $ ty) :: Maybe Int
  pure (x, y)
{-! SECTION> 01_parseScreenSize !-}

