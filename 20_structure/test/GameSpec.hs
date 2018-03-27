{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module GameSpec where

import Protolude 
import qualified Data.Map.Strict as Map
import           Test.Hspec
import           Test.QuickCheck

import           GameCore
import qualified EntityType as E
import qualified GameEngine as GE

mkEntityAndTile :: Int -> Text -> (Int, Int) -> (Entity, Tile)
mkEntityAndTile id name pic =
  let t = Tile { _tlId = id, _tlName = name, _tlPic = pic } in
  let a = Entity { _enType = E.Blank, _enTile = t, _enProps = Map.empty} in
  (a, t)
    

tileBlank  :: Tile
tileWall   :: Tile
tilePlayer :: Tile

entityBlank  :: Entity
entityWall   :: Entity
entityPlayer :: Entity

(entityBlank , tileBlank ) = mkEntityAndTile 1 "blank"  (1, 1) 
(entityWall  , tileWall  ) = mkEntityAndTile 2 "wall"   (2, 2)
(entityPlayer, tilePlayer) = mkEntityAndTile 3 "player" (3, 3)

csvSmall :: Text
csvSmall = " , , ,+, \n\
           \ , , , , \n\
           \ ,+, , ,+\n\
           \ , , , , \n\
           \ , , ,+, "

spec :: Spec
spec = do
  describe "co-ords basics" $ do
    it "world to player: on (0,0) - 1" $
      GE.worldCoordToPlayer (WorldPos (0, 0)) (WorldPos (0, 0)) `shouldBe` PlayerPos (0, 0)
    it "world to player: on (0,0) - 2" $
      GE.worldCoordToPlayer (WorldPos (0, 0)) (WorldPos (1, 0)) `shouldBe` PlayerPos (1, 0)
    it "world to player: on (0,0) - 3" $
      GE.worldCoordToPlayer (WorldPos (0, 0)) (WorldPos (4, 0)) `shouldBe` PlayerPos (4, 0)
    it "world to player: on (0,0) - 4" $
      GE.worldCoordToPlayer (WorldPos (0, 0)) (WorldPos (0, 1)) `shouldBe` PlayerPos (0, -1)
    it "world to player: on (0,0) - 5" $
      GE.worldCoordToPlayer (WorldPos (0, 0)) (WorldPos (0, 4)) `shouldBe` PlayerPos (0, -4)

    it "world to player: not (0,0) - 1" $
      GE.worldCoordToPlayer (WorldPos (2, -4)) (WorldPos (2, -4)) `shouldBe` PlayerPos (0, 0)
    it "world to player: not (0,0) - 2" $
      GE.worldCoordToPlayer (WorldPos (2, -4)) (WorldPos (6, -7)) `shouldBe` PlayerPos (4, 3)


    it "player to world: on (0,0) - 1" $
      GE.playerCoordToWorld (WorldPos (0, 0)) (PlayerPos (0, 0)) `shouldBe` WorldPos (0, 0)
    it "player to world: on (0,0) - 2" $
      GE.playerCoordToWorld (WorldPos (0, 0)) (PlayerPos (1, 0)) `shouldBe` WorldPos (1, 0)
    it "player to world: on (0,0) - 3" $
      GE.playerCoordToWorld (WorldPos (0, 0)) (PlayerPos (4, 0)) `shouldBe` WorldPos (4, 0)
    it "player to world: on (0,0) - 4" $
      GE.playerCoordToWorld (WorldPos (0, 0)) (PlayerPos (0, -1)) `shouldBe` WorldPos (0, 1)
    it "player to world: on (0,0) - 5" $
      GE.playerCoordToWorld (WorldPos (0, 0)) (PlayerPos (0, -4)) `shouldBe` WorldPos (0, 4)

    it "player to world: not (0,0) - 1" $
      GE.playerCoordToWorld (WorldPos (2, -4)) (PlayerPos (0, 0)) `shouldBe` WorldPos (2, -4)
    it "player to world: not (0,0) - 2" $
      GE.playerCoordToWorld (WorldPos (2, -4)) (PlayerPos (4, 3)) `shouldBe` WorldPos (6, -7)

    it "player to world to player" $ property $
      \x y tx ty -> do
        let (WorldPos (wx, wy)) = GE.playerCoordToWorld (WorldPos (tx, ty)) (PlayerPos (x, y))
        let (PlayerPos (px, py)) = GE.worldCoordToPlayer (WorldPos (tx, ty)) (WorldPos (wx, wy))
        (px, py) `shouldBe` (x, y)

    it "world to player to world" $ property $
      \x y tx ty -> do
        let (PlayerPos (px, py)) = GE.worldCoordToPlayer (WorldPos (tx, ty)) (WorldPos (x, y))
        let (WorldPos (wx, wy)) = GE.playerCoordToWorld (WorldPos (tx, ty)) ( PlayerPos(px, py))
        (wx, wy) `shouldBe` (x, y)


  describe "parse world co-ords" $ do
    let parsed = GE.parseWorld (Map.fromList [("+", entityWall)]) csvSmall

    it "parsed to player pos" $ do
      let p = Map.toList parsed
      p `shouldMatchList` [ (PlayerPos (3, 0), entityWall)
                          , (PlayerPos (1, 2), entityWall)
                          , (PlayerPos (4, 2), entityWall)
                          , (PlayerPos (3, 4), entityWall)
                          ]

    it "translatePlayerMap to (0, 0)" $ do
      let wm = GE.translatePlayerMap (WorldPos (0, 0)) parsed
      let p = Map.toList wm
      p `shouldMatchList` [ (WorldPos (3,  0), entityWall)
                          , (WorldPos (1, -2), entityWall)
                          , (WorldPos (4, -2), entityWall)
                          , (WorldPos (3, -4), entityWall)
                          ]
