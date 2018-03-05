{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module GameHost ( runHost
                , Connection(..)
                , conSendData
                , conReceiveText
                ) where

{-! SECTION< 01_include !-}
import Protolude hiding (Map)
import qualified Web.Scotty as Sc
import qualified Data.ByteString.Lazy as BSL
import           Control.Lens.TH (makeLenses)
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Gzip as Sc
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWs
import qualified Network.WebSockets as WS
{-! SECTION> 01_include !-}

{-! SECTION< 01_connection !-}
data Connection = Connection { _conSendData :: BSL.ByteString -> IO ()
                             , _conReceiveText :: IO Text
                             }

makeLenses ''Connection
{-! SECTION> 01_connection !-}

{-! SECTION< 01_runHost !-}
runHost :: (Connection -> IO ()) -> IO ()
runHost startHost = do
  let settings = Warp.setPort 61492 $ Warp.setHost "127.0.0.1" Warp.defaultSettings
  sapp <- scottyApp 
  Warp.runSettings settings $ WaiWs.websocketsOr WS.defaultConnectionOptions (wsapp startHost) sapp
{-! SECTION> 01_runHost !-}

{-! SECTION< 01_scottyApp !-}
scottyApp :: IO Wai.Application
scottyApp = 
  Sc.scottyApp $ do
    Sc.middleware $ Sc.gzip $ Sc.def { Sc.gzipFiles = Sc.GzipCompress }
    --Sc.middleware S.logStdoutDev

    Sc.get "/" $
      Sc.file "html/rogue.html"

    Sc.get "/ping" $
      Sc.text "pong"

    Sc.get "/:res" $ do
      res <- Sc.param "res"
      Sc.file $ "html/" <> res

    Sc.get "/images/:img" $ do
      img <- Sc.param "img"
      Sc.file $ "html/images/" <> img
{-! SECTION> 01_scottyApp !-}

  
{-! SECTION< 01_wsapp !-}
wsapp :: (Connection -> IO ()) -> WS.ServerApp
wsapp startHost pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30

  startHost Connection { _conSendData = WS.sendBinaryData conn
                       , _conReceiveText = WS.receiveData conn
                       }
{-! SECTION> 01_wsapp !-}
