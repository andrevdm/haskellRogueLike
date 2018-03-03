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
                | Unknown
                deriving (Show, Eq, Ord)
