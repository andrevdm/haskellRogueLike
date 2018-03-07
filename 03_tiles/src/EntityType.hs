{-# LANGUAGE NoImplicitPrelude #-}

module EntityType where

import Protolude

{-! SECTION< 03_entityType !-}
data EntityType = Blank
                | Door
                | DoorClosed
                | Wall
                | Unknown
                deriving (Show, Eq, Ord)
{-! SECTION> 03_entityType !-}
