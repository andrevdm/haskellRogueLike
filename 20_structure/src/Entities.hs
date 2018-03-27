{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Entities where

import Protolude hiding (Map)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           GameCore
import qualified EntityType as E

tiles :: Map E.EntityType Tile
entities :: Map E.EntityType Entity
loadTexts :: Map Text Entity

(tiles, entities, loadTexts) =
  let is = [ (E.Blank     , (41, 13), Nothing)
           , (E.Door      , (26, 15), Just "+")
           , (E.DoorClosed, (21, 15), Just "-")
           , (E.Wall      , ( 9, 14), Just "w")
           , (E.Player    , ( 8,  3), Nothing)
           , (E.Bug       , (25,  3), Nothing)
           , (E.Snake     , (38,  4), Nothing)
           , (E.Dark      , (43, 11), Nothing)
           , (E.Stairs    , (56, 44), Just "s")
           ]
  in
  let mkData (typ, pos@(x, y), l) (tiles', entities', loads') =
        let (entity, tile) = mkEntityAndTile (x * 100 + y) typ pos in
        ( Map.insert typ tile tiles'
        , Map.insert typ entity entities'
        , maybe loads' (\load -> Map.insert load entity loads') l
        )
  in
  foldr
    mkData
    ( Map.fromList [(E.Unknown, tileUnknown)]
    , Map.fromList [(E.Unknown, entityUnknown)]
    , Map.empty
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
