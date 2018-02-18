{-# LANGUAGE NoImplicitPrelude #-}

module EntityType where

import Protolude

data EntityType = Blank
                | Door
                | DoorClosed
                | Wall
                | Unknown
                deriving (Show, Eq, Ord)
