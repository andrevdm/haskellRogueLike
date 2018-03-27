{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GameHost ( runHost
                , Connection(..)
                , conSendData
                , conReceiveText
                , HostState
                , HostT (..)
                ) where

import Protolude hiding (Map)
import qualified Web.Scotty as Sc
import qualified Data.ByteString.Lazy as BSL
import           Control.Lens.TH (makeLenses)
import           Control.Monad.Trans (lift)
import           Control.Monad.Reader (runReaderT, ReaderT, MonadReader, MonadTrans)
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Gzip as Sc
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWs
import qualified Network.WebSockets as WS

----------------------------------------------------------------------------------------------------------------
-- L1 Host (orchestration)
----------------------------------------------------------------------------------------------------------------
type HostState = Maybe Connection 

newtype HostT m a = HostT { unHostT :: ReaderT HostState m a 
                          } deriving (Functor, Applicative, Monad, MonadReader HostState, MonadTrans)

data Connection = Connection { _conSendData :: BSL.ByteString -> IO ()
                             , _conReceiveText :: IO Text
                             }

makeLenses ''Connection

runHost :: HostT IO () -> HostT IO ()
runHost startHost = do
  let settings = Warp.setPort 61492 $ Warp.setHost "127.0.0.1" Warp.defaultSettings
  sapp <- scottyApp 
  lift . Warp.runSettings settings $ WaiWs.websocketsOr WS.defaultConnectionOptions (wsapp startHost) sapp


scottyApp :: HostT IO Wai.Application
scottyApp = 
  lift . Sc.scottyApp $ do
    Sc.middleware $ Sc.gzip $ Sc.def { Sc.gzipFiles = Sc.GzipCompress }
    --Sc.middleware S.logStdoutDev

    Sc.get "/" $
      Sc.file "html/rogue.html"

    Sc.get "/ping" $
      Sc.text "pong"

    Sc.get "/tiles.png" $
      Sc.file "html/DungeonCrawl_ProjectUtumnoTileset_0.png"

    Sc.get "/:res" $ do
      res <- Sc.param "res"
      Sc.file $ "html/" <> res

    Sc.get "/images/:img" $ do
      img <- Sc.param "img"
      Sc.file $ "html/images/" <> img

  
wsapp :: HostT IO () -> WS.ServerApp
wsapp startHost pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30

  let h = Connection { _conSendData = WS.sendBinaryData conn
                     , _conReceiveText = WS.receiveData conn
                     }
  runReaderT (unHostT startHost) (Just h)
----------------------------------------------------------------------------------------------------------------
