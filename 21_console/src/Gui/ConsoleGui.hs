{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Gui.ConsoleGui (runGui) where

import           Protolude
import qualified Data.Map.Strict as Map
import qualified Data.Text as Txt
import qualified Data.Text.Encoding as TxtE
import qualified Data.Aeson as Ae
import qualified Data.ByteString.Lazy as BSL
import qualified Codec.Compression.BZip as Bz
import qualified System.IO as IO
import qualified System.Console.ANSI as A
import qualified System.Console.Terminal.Size as Sz
import           Control.Exception.Safe (throwString)
import qualified Network.Socket as Sock
import qualified Network.WebSockets as WS

import qualified GameCore as GC


sendCommand :: WS.Connection -> Text -> IO ()
sendCommand = WS.sendTextData

receiveCommand :: WS.Connection -> IO Text
receiveCommand conn = do
  compressed <- WS.receiveData conn
  pure . TxtE.decodeUtf8 . BSL.toStrict $ Bz.decompress compressed

close :: WS.Connection -> IO ()
close conn = WS.sendClose conn ("" :: Text)


{-! SECTION< 21_runGui !-}
runGui :: IO ()
runGui = do
  IO.hSetEcho stdin False
  IO.hSetBuffering stdin IO.NoBuffering
  IO.hSetBuffering stdout IO.NoBuffering
  
  Sock.withSocketsDo $ WS.runClient "localhost" 61492 "/" app

  where
    app :: WS.Connection -> IO ()
    app conn = do
      (x, y) <- getSize
      runConnection conn x y 
{-! SECTION> 21_runGui !-}


{-! SECTION< 21_runConnection !-}
runConnection :: WS.Connection -> Int -> Int -> IO ()
runConnection conn x y = do
  sendCommand conn $ "init|" <> show x <> "|" <> show y <> ""
  void $ forkIO (runKeys conn)
  void loop
  close conn

  where
    loop = do
      cmd <- receiveCommand conn
      handleCommand conn cmd
      loop
{-! SECTION> 21_runConnection !-}

{-! SECTION< 21_keys !-}
runKeys :: WS.Connection -> IO ()
runKeys conn = do
  key <- getKey >>= \case
       "k"      -> pure "Move:up"
       "\ESC[A" -> pure "Move:up"

       "j"      -> pure "Move:down"
       "\ESC[B" -> pure "Move:down"

       "l"      -> pure "Move:right"
       "\ESC[C" -> pure "Move:right"

       "h"      -> pure "Move:left"
       "\ESC[D" -> pure "Move:left"

       _        -> pure ""
  
  sendCommand conn $ "key|" <> key
  runKeys conn

-- | https://stackoverflow.com/questions/23068218/haskell-read-raw-keyboard-input
getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- IO.getChar
          more <- IO.hReady stdin
          (if more then getKey' else return) (char:chars)
{-! SECTION> 21_keys !-}


{-! SECTION< 21_handleCommand !-}
handleCommand :: WS.Connection -> Text -> IO ()
handleCommand conn cmd' =
  case Ae.eitherDecode (BSL.fromStrict . TxtE.encodeUtf8 $ cmd') :: Either [Char] CommandWrapper of
    Left e -> throwString e
    Right cmd ->
     case cmd of
         CommandWrapper "log" -> pass
         CommandWrapper "error" -> pass
         CommandWrapper "config" -> handleConfig conn cmd'
         CommandWrapper "draw" -> handleDraw conn cmd'
         _ -> throwString . Txt.unpack $ "Unknown command: " <> cmd'


handleConfig :: WS.Connection -> Text -> IO ()
handleConfig conn cmd' =
  case Ae.eitherDecode (BSL.fromStrict . TxtE.encodeUtf8 $ cmd') :: Either [Char] GC.UiConfig of
    Left e -> throwString e
    Right _cmd -> do
      (w,h) <- getSize
      sendCommand conn $ "redraw|" <> show w <> "|" <> show h
      pass
{-! SECTION> 21_handleCommand !-}


{-! SECTION< 21_drawing !-}
handleDraw :: WS.Connection -> Text -> IO ()
handleDraw _conn cmd' =
  case Ae.eitherDecode (BSL.fromStrict . TxtE.encodeUtf8 $ cmd') :: Either [Char] GC.UiDrawCommand of
    Left e -> throwString e
    Right cmd -> do
      (width, height) <- getSize
      
      let l1 = (\(x, y, i) -> ((x, y), i)) <<$>> GC.drMapData cmd
      let layers = Map.unions . reverse $ Map.fromList <$> l1

      A.setSGR [A.Reset]
      A.clearScreen
      A.hideCursor

      A.setCursorPosition 0 0
      let (t, s) = tileFromId 4113
      let blankLine = Txt.replicate width t
      A.setSGR s
      traverse_ putText $ replicate height blankLine
      traverse_ drawTile $ Map.toList layers
      A.setSGR [A.Reset]

  where
    drawTile :: ((Int, Int), Int) -> IO ()
    drawTile ((x, y), tid) = do
      A.setCursorPosition y x
      let (t, s) = tileFromId tid
      A.setSGR s
      putStr t
{-! SECTION> 21_drawing !-}
      

{-! SECTION< 21_getSize !-}
getSize :: IO (Int, Int)
getSize =
  Sz.size @ Int >>= \case
    Nothing -> throwString "unable to get screen size"
    Just (Sz.Window w h) -> pure (min 50 w, min 20 h)
{-! SECTION> 21_getSize !-}
      
{-! SECTION< 21_tiles !-}
-- | https://github.com/globalcitizen/zomia/blob/master/USEFUL-UNICODE.md
tileFromId :: Int -> (Text, [A.SGR])
tileFromId 4113 = (" ", []) -- " " -- E.Blank
tileFromId 2615 = ("⌺", [A.SetColor A.Foreground A.Vivid A.Green]) -- "'" -- E.Door
tileFromId 2115 = ("⊠", [A.SetColor A.Foreground A.Vivid A.Red]) -- "+" -- E.DoorClosed
tileFromId  914 = ("█", [A.SetColor A.Foreground A.Dull A.White]) -- "#" -- E.Wall
tileFromId  803 = ("Ӧ", [A.SetColor A.Foreground A.Vivid A.Cyan]) -- "@" -- E.Player
tileFromId 2503 = ("⍾", [A.SetColor A.Foreground A.Dull A.Magenta]) -- "B" -- E.Bug
tileFromId 3804 = ("ຯ", [A.SetColor A.Foreground A.Vivid A.Magenta]) -- "S" -- E.Snake
tileFromId 4311 = ("░", [A.SetColor A.Foreground A.Dull A.White]) -- "░" -- E.Dark
tileFromId 5644 = ("ᝐ", [A.SetColor A.Foreground A.Dull A.Green]) -- ">" -- E.Stairs
tileFromId 1646 = ("☁", [A.SetColor A.Foreground A.Dull A.White]) -- "d" -- E.PotionDark
tileFromId  846 = ("☀", [A.SetColor A.Foreground A.Vivid A.Yellow]) -- "l" -- E.PotionLight
tileFromId 5445 = ("ዋ", [A.SetColor A.Foreground A.Dull A.Green]) -- "k" -- E.Key
tileFromId _    = ("?", [])
{-! SECTION> 21_tiles !-}

--------------------------------------------------------------------------------------------------------------------
data CommandWrapper = CommandWrapper
                      { coCmd :: !Text
                      } deriving (Generic, Show)


instance Ae.FromJSON CommandWrapper where
  parseJSON = Ae.genericParseJSON Ae.defaultOptions { Ae.fieldLabelModifier = renField 2 True }


-- | drop prefix, and then lower case
-- | renField 3 "tskBla" == "bla"
renField :: Int -> Bool -> [Char] -> [Char]
renField drp toLower =
  Txt.unpack . (if toLower then mkLower else identity) . Txt.drop drp . Txt.pack
  where
    mkLower t = Txt.toLower (Txt.take 1 t) <> Txt.drop 1 t
