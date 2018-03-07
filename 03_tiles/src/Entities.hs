{-# LANGUAGE NoImplicitPrelude #-}

module Entities where

import Protolude hiding (Map)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           GameCore
import qualified EntityType as E

{-! SECTION< 03_entities !-}
tiles :: Map E.EntityType Tile
entities :: Map E.EntityType Entity
{-! SECTION< 03_entities !-}

(tiles, entities) =
  let is = [ (E.Blank     , (41, 13))
           , (E.Door      , (26, 15))
           , (E.DoorClosed, (21, 15))
           , (E.Wall      , ( 9, 14))
           ]
  in
  let mkData (typ, pos@(x, y)) (tiles', entities') =
        let (entity, tile) = mkEntityAndTile (x * 100 + y) typ pos in
        ( Map.insert typ tile tiles'
        , Map.insert typ entity entities'
        )
  in
  foldr
    mkData
    ( Map.fromList [(E.Unknown, tileUnknown)]
    , Map.fromList [(E.Unknown, entityUnknown)]
    )
    is


getEntity :: E.EntityType -> Entity
getEntity e = Map.findWithDefault entityUnknown e entities

getTile :: E.EntityType -> Tile
getTile e = Map.findWithDefault tileUnknown e tiles


mkEntityAndTile :: Int -> E.EntityType -> (Int, Int) -> (Entity, Tile)
mkEntityAndTile id typ pic =
  let t = Tile { _tlId = id, _tlName = show typ, _tlPic = pic } in
  let a = Entity { _enType = typ, _enTile = t, _enProps = Map.empty} in
  (a, t)
    

tileUnknown :: Tile
entityUnknown :: Entity
(entityUnknown, tileUnknown) = mkEntityAndTile 201 E.Unknown (2, 1)
