{-# LANGUAGE NoImplicitPrelude #-}

module EntityType where

import Protolude

{-! SECTION< 18_entityType !-}
data EntityType = Blank
                | Door
                | DoorClosed
                | Wall
                | Player
                | Bug
                | Snake
                | Dark
                | Stairs
                | Unknown
                deriving (Show, Eq, Ord)
{-! SECTION> 18_entityType !-}
