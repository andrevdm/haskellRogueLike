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
import qualified System.Random as Rnd
import           Control.Lens.TH (makeLenses)

import qualified GameHost as Host
import qualified EntityType as E

data ActorClass = ClassPlayer
                | ClassEnemy
                deriving (Show, Eq)

newtype Aid = Aid Text deriving (Show, Eq, Ord)

data Actor = Actor { _acId :: !Aid
                   , _acClass :: !ActorClass
                   , _acEntity :: !Entity
                   , _acWorldPos :: !WorldPos
                   , _acStdGen :: !Rnd.StdGen
                   }

data Player = Player { _plConn :: !Host.Connection
                     , _plActor :: !Actor
                     , _plScreenSize :: !(Int, Int)
                     , _plWorldTopLeft :: !WorldPos
                     }

data World = World { _wdPlayer :: !Player
                   , _wdConfig :: !Config
                   , _wdMap :: !(Map WorldPos Entity)
                   , _wdActors :: !(Map Aid Actor)
                   }

data Config = Config { _cfgKeys :: !(Map Text Text)
                     , _cfgMinMaxBounds :: !(Int, Int, Int, Int) -- (minX, maxX, minY, maxY)
                    }


data Tile = Tile { _tlName :: !Text
                 , _tlPic :: !(Int, Int)
                 , _tlId :: !Int
                 } deriving (Show, Eq, Ord)

data Entity = Entity { _enType :: !E.EntityType
                     , _enTile :: !Tile
                     , _enProps :: !(Map Text Text)
                     , _enAttribs :: !(Map Text Int)
                     } deriving (Show, Eq, Ord)

newtype WorldPos = WorldPos (Int, Int) deriving (Show, Eq, Ord)
newtype PlayerPos = PlayerPos (Int, Int) deriving (Show, Eq, Ord)

newtype RogueAction = ActMovePlayer (Int, Int)


data UiMessage = UiMessage { umCmd :: !Text
                           , umMessage :: !Text
                           }
                           deriving (Generic)
  
data UiConfig = UiConfig { ucCmd :: !Text
                         , ucData :: !UiConfigData
                         }
                         deriving (Generic)

data UiConfigData = UiConfigData { udKeys :: ![UiKey]
                                 , udBlankId :: !Int
                                 }
                                 deriving (Generic)

data UiKey = UiKey { ukShortcut :: !Text
                   , ukAction :: !Text
                   }
                   deriving (Generic)


data UiDrawCommand = UiDrawCommand
                     { drCmd :: !Text
                     , drScreenWidth :: !Int
                     , drMapData :: ![(Int, Int, Int)]
                     } deriving (Generic)


instance Ae.ToJSON UiMessage where
  toJSON = Ae.genericToJSON Ae.defaultOptions { Ae.fieldLabelModifier = renField 2 True }

instance Ae.ToJSON UiConfig where
  toJSON = Ae.genericToJSON Ae.defaultOptions { Ae.fieldLabelModifier = renField 2 True }

instance Ae.ToJSON UiConfigData where
  toJSON = Ae.genericToJSON Ae.defaultOptions { Ae.fieldLabelModifier = renField 2 True }

instance Ae.ToJSON UiKey where
  toJSON = Ae.genericToJSON Ae.defaultOptions { Ae.fieldLabelModifier = renField 2 True }

instance Ae.ToJSON UiDrawCommand where
  toJSON = Ae.genericToJSON Ae.defaultOptions { Ae.fieldLabelModifier = renField 2 True }


-- | drop prefix, and then lower case
-- | renField 3 "tskBla" == "bla"
renField :: Int -> Bool -> [Char] -> [Char]
renField drp toLower =
  Txt.unpack . (if toLower then mkLower else identity) . Txt.drop drp . Txt.pack
  where
    mkLower t = Txt.toLower (Txt.take 1 t) <> Txt.drop 1 t


makeLenses ''World
makeLenses ''Config
makeLenses ''Player
makeLenses ''Entity
makeLenses ''Tile
makeLenses ''Actor