{-# LANGUAGE NoImplicitPrelude #-}

module EntityType where

import Protolude

data EntityType = Blank
                | Door
                | DoorClosed
                | Wall
                | Player
                | Bug
                | Snake
                | Dark
                | Stairs
                | PotionDark
                | PotionLight
                | Key
                | Unknown
                deriving (Show, Eq, Ord)
