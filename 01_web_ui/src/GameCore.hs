{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GameCore where

import           Protolude hiding (Map)
import qualified Data.Text as Txt
import           Data.Map.Strict (Map)
import qualified Data.Aeson as Ae
import           Control.Lens.TH (makeLenses)

import qualified GameHost as Host


data Player = Player { _plConn :: Host.Connection
                     , _plScreenSize :: !(Int, Int)
                     }

data World = World { _wdPlayer :: !Player
                   , _wdConfig :: !Config
                   }

data Config = Config { _cfgKeys :: !(Map Text Text)
                     }

makeLenses ''World
makeLenses ''Config
makeLenses ''Player


data UiMessage = UiMessage { umCmd :: Text
                           , umMessage :: Text
                           }
                           deriving (Generic)
  
data UiConfig = UiConfig { ucCmd :: Text
                         , ucData :: UiConfigData
                         }
                         deriving (Generic)

data UiConfigData = UiConfigData { udKeys :: [UiKey]
                                 }
                                 deriving (Generic)

data UiKey = UiKey { ukShortcut :: Text
                   , ukAction :: Text
                   }
                   deriving (Generic)


instance Ae.ToJSON UiMessage where
  toJSON = Ae.genericToJSON Ae.defaultOptions { Ae.fieldLabelModifier = renField 2 True }

instance Ae.ToJSON UiConfig where
  toJSON = Ae.genericToJSON Ae.defaultOptions { Ae.fieldLabelModifier = renField 2 True }

instance Ae.ToJSON UiConfigData where
  toJSON = Ae.genericToJSON Ae.defaultOptions { Ae.fieldLabelModifier = renField 2 True }

instance Ae.ToJSON UiKey where
  toJSON = Ae.genericToJSON Ae.defaultOptions { Ae.fieldLabelModifier = renField 2 True }


-- | drop prefix, and then lower case
-- | renField 3 "tskBla" == "bla"
renField :: Int -> Bool -> [Char] -> [Char]
renField drp toLower =
  Txt.unpack . (if toLower then mkLower else identity) . Txt.drop drp . Txt.pack
  where
    mkLower t = Txt.toLower (Txt.take 1 t) <> Txt.drop 1 t
