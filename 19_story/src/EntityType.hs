{-# LANGUAGE NoImplicitPrelude #-}

module EntityType where

import Protolude

{-! SECTION< 19_entityTypes !-}
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
{-! SECTION> 19_entityTypes !-}
