{-# LANGUAGE NoImplicitPrelude #-}

module Memory ( Memory
              , empty
              , remember
              , recall
              , forget
              , forgetAll
              , remembers
              , tick
              , toList
              , fromList
              ) where

import Protolude hiding (Map, empty, toList)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

-- | Store (remembers) values for a given period (measured by ticks)
newtype Memory a = Memory (Map Text (Map a Int)) 


empty :: Memory a
empty = Memory Map.empty


remember :: (Ord a) => (Int -> Int -> Int) -> Text -> Int -> a -> Memory a -> Memory a
remember combineFn key ttl val (Memory m) =
  Memory $ Map.alter ins key m

  where
    ins Nothing = Just $ Map.singleton val ttl
    ins (Just vs) = Just $ Map.insertWith combineFn val ttl vs


recall :: Text -> Memory a -> Map a Int
recall key (Memory m) =
  fromMaybe Map.empty $ Map.lookup key m 


forgetAll :: Text -> Memory a -> Memory a
forgetAll key (Memory m) =
  Memory $ Map.delete key m


forget :: (Ord a) => Text -> a -> Memory a -> Memory a
forget key val (Memory m) =
  Memory $ Map.alter rm key m

  where
    rm Nothing = Nothing
    rm (Just vs) = Just $ Map.delete val vs


remembers :: (Ord a) => Text -> a -> Memory a -> Bool
remembers key val m =
  Map.member val (recall key m)


tick :: (Ord a) => Memory a -> Memory a
tick (Memory m) =
  Memory $ tickBucket <$> m
  
  where
    tickBucket :: (Ord a) => Map a Int -> Map a Int
    tickBucket vs =
      foldr (Map.alter tickVal) vs (Map.keys vs)

    tickVal :: Maybe Int -> Maybe Int
    tickVal Nothing = Nothing
    tickVal (Just i) = if i - 1 <= 0 then Nothing else Just (i - 1)
    

toList :: Memory a -> [(Text, [(a, Int)])]
toList (Memory m) = Map.toList $ Map.toList <$> m


fromList :: (Ord a) => [(Text, [(a, Int)])] -> Memory a
fromList vs = Memory . Map.fromList $ Map.fromList <<$>> vs
