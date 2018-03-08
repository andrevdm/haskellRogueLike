{-# LANGUAGE NoImplicitPrelude #-}

module EntityType where

import Protolude

{-! SECTION< 05_entityType !-}
data EntityType = Blank
                | Door
                | DoorClosed
                | Wall
                | Player
                | Bug
                | Snake
                | Unknown
                deriving (Show, Eq, Ord)
{-! SECTION> 05_entityType !-}
