{-# LANGUAGE NoImplicitPrelude #-}

{-! SECTION< 15_exports !-}
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
{-! SECTION> 15_exports !-}

import Protolude hiding (Map, empty, toList)
{-! SECTION< 15_mem_type !-}
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

-- | Store (remembers) values for a given period (measured by ticks)
newtype Memory a = Memory (Map Text (Map a Int)) 
{-! SECTION> 15_mem_type !-}


{-! SECTION< 15_empty !-}
empty :: Memory a
empty = Memory Map.empty
{-! SECTION> 15_empty !-}


{-! SECTION< 15_remember !-}
remember :: (Ord a) => (Int -> Int -> Int) -> Text -> Int -> a -> Memory a -> Memory a
remember combineFn key ttl val (Memory m) =
  Memory $ Map.alter ins key m

  where
    ins Nothing = Just $ Map.singleton val ttl
    ins (Just vs) = Just $ Map.insertWith combineFn val ttl vs
{-! SECTION> 15_remember !-}


{-! SECTION< 15_recall !-}
recall :: Text -> Memory a -> Map a Int
recall key (Memory m) =
  fromMaybe Map.empty $ Map.lookup key m 
{-! SECTION> 15_recall !-}


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


{-! SECTION< 15_tick !-}
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
{-! SECTION> 15_tick !-}
    

toList :: Memory a -> [(Text, [(a, Int)])]
toList (Memory m) = Map.toList $ Map.toList <$> m


fromList :: (Ord a) => [(Text, [(a, Int)])] -> Memory a
fromList vs = Memory . Map.fromList $ Map.fromList <<$>> vs
