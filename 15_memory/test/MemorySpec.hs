{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module MemorySpec where

import Protolude 
import qualified Data.Map as Map
import           Test.Hspec

import qualified Memory as M

spec :: Spec
spec = do
  describe "simple" $ do
    it "nothing to recall" $ do
      let m = M.empty
      M.remembers "test" 0 m `shouldBe` False

    it "remembers" $ do
      let m = M.remember (\_ n -> n) "test" 1 (99::Int) M.empty
      M.remembers "test" 99 m `shouldBe` True

    it "forgets all" $ do
      let m = M.forgetAll "test" $ M.remember (\_ n -> n) "test" 1 (99::Int) M.empty
      M.remembers "test" 99 m `shouldBe` False

    it "forgets " $ do
      let m = M.forget "test" 99 $ M.remember (\_ n -> n) "test" 1 (99::Int) M.empty
      M.remembers "test" 99 m `shouldBe` False

    it "does not remember, wrong key" $ do
      let m = M.remember (\_ n -> n) "test" 1 "val99" M.empty
      M.remembers "!test" "val99" m `shouldBe` False

    it "does not remember, wrong value" $ do
      let m = M.remember (\_ n -> n) "test" 1 "val99" M.empty
      M.remembers "test" "val199" m `shouldBe` False

    it "tick removes" $ do
      let m = M.remember (\_ n -> n) "test" 1 "val99" M.empty
      M.remembers "test" "val99" (M.tick m) `shouldBe` False

  describe "multi" $ do
    let orig = [ ("k1", [("k1.a" :: Text, 2), ("k1.b", 3)])
               , ("k2", [("k2.a", 3)])
               ]

    let m = M.fromList orig

    it "toList" $ 
      M.toList m `shouldBe` orig
    
    it "remembers" $ do
      M.remembers "k1" "k1.a" m `shouldBe` True
      M.remembers "k1" "k1.b" m `shouldBe` True
      M.remembers "k2" "k2.a" m `shouldBe` True

      M.remembers "k2" "k1.a" m `shouldBe` False
      M.remembers "k2" "k1.b" m `shouldBe` False
      M.remembers "k1" "k2.a" m `shouldBe` False

    it "recall" $ do
      M.recall "k1" m `shouldBe` Map.fromList [("k1.a", 2), ("k1.b", 3)]
      M.recall "k2" m `shouldBe` Map.fromList [("k2.a", 3)]
      M.recall "k3" m `shouldBe` Map.empty

    it "tick" $ do
      let m1 = M.tick m
      M.recall "k1" m1 `shouldBe` Map.fromList [("k1.a", 1), ("k1.b", 2)]
      M.recall "k2" m1 `shouldBe` Map.fromList [("k2.a", 2)]
      M.recall "k3" m1 `shouldBe` Map.empty

      let m2 = M.tick m1
      M.recall "k1" m2 `shouldBe` Map.fromList [("k1.b", 1)]
      M.recall "k2" m2 `shouldBe` Map.fromList [("k2.a", 1)]
      M.recall "k3" m2 `shouldBe` Map.empty

      let m3 = M.tick m2
      M.recall "k1" m3 `shouldBe` Map.empty
      M.recall "k2" m3 `shouldBe` Map.empty
      M.recall "k3" m3 `shouldBe` Map.empty

  describe "combine fn" $ do
    it "add" $ do
      let m1 = M.remember (+) "bucket1" 2 "val1" M.empty
      let m2 = M.remember (+) "bucket1" 3 "val1" m1
      M.recall "bucket1" m2 `shouldBe` Map.singleton "val1" 5

    it "don't replace" $ do
      let m1 = M.remember (\_ old -> old) "bucket1" 2 "val1" M.empty
      let m2 = M.remember (\_ old -> old) "bucket1" 3 "val1" m1
      M.recall "bucket1" m2 `shouldBe` Map.singleton "val1" 2

    it "replace" $ do
      let m1 = M.remember const "bucket1" 2 "val1" M.empty
      let m2 = M.remember const "bucket1" 3 "val1" m1
      M.recall "bucket1" m2 `shouldBe` Map.singleton "val1" 3

    it "min" $ do
      let m1 = M.remember min "bucket1" 2 "val1" M.empty
      let m2 = M.remember min "bucket1" 3 "val1" m1
      M.recall "bucket1" m2 `shouldBe` Map.singleton "val1" 2

    it "max" $ do
      let m1 = M.remember max "bucket1" 2 "val1" M.empty
      let m2 = M.remember max "bucket1" 3 "val1" m1
      M.recall "bucket1" m2 `shouldBe` Map.singleton "val1" 3


