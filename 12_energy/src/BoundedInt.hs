{-# LANGUAGE NoImplicitPrelude #-}

{-! SECTION< 12_boundedInt !-}
module BoundedInt ( BInt
                  , new
                  , update
                  , set
                  , get
                  , getMax
                  ) where

import Protolude hiding (maxBound, get)

newtype BInt = BInt (Int, Int) deriving (Eq, Show)

new :: Int -> Int -> BInt
new maxBound v =
  BInt (maxBound, max 0 $ min maxBound v)


set :: Int -> BInt -> BInt
set newValue (BInt (maxBound, _)) =
  BInt (maxBound, max 0 $ min maxBound newValue)


get :: BInt -> Int
get (BInt (_, v)) =
  v


getMax :: BInt -> Int
getMax (BInt (m, _)) =
  m


update :: (Int -> Int) -> BInt -> BInt
update fn (BInt (maxBound, v)) =
  BInt (maxBound, max 0 . min maxBound $ fn v)
{-! SECTION> 12_boundedInt !-}
