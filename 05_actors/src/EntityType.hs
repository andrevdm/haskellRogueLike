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
                | Unknown
                deriving (Show, Eq, Ord)
