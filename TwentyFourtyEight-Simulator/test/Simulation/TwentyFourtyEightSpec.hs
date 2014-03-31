module Simulation.TwentyFourtyEightSpec (main, spec) where

import qualified Data.Matrix as M
import qualified Data.Set as S
import Data.Maybe
import Test.Hspec
import Simulation.TwentyFourtyEight


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Shifting right" $ do
    it "Should take the a matrix and combine all similar numbers and shift them all to the right" $ do
      let a = M.fromLists [[Nothing, (Just 2), (Just 2), (Just 2)]
                           ,[Nothing, (Just 2), (Just 2), Nothing]
                           ,[Nothing, Nothing, (Just 4), Nothing]
                           ,[Nothing, (Just 2), (Just 4), Nothing]]
          b = M.fromLists [[Nothing, Nothing, (Just 2) , (Just 4)]
                           ,[Nothing, Nothing, Nothing, (Just 4)]
                           ,[Nothing, Nothing,  Nothing, (Just 4)]
                           ,[Nothing, Nothing, (Just 2), (Just 4)]]
          shifted = shiftRight a
      shifted `shouldBe` b
  describe "Shifting left" $ do
    it "Should take the a matrix and combine all similar numbers and shift them all to the left" $ do
      let a = M.fromLists [[Nothing, (Just 2), (Just 2), (Just 2)]
                           ,[Nothing, (Just 2), (Just 2), Nothing]
                           ,[Nothing, Nothing, (Just 4), Nothing]
                           ,[Nothing, (Just 2), (Just 4), Nothing]]
          b = M.fromLists [[(Just 4), (Just 2), Nothing , Nothing]
                           ,[(Just 4), Nothing, Nothing, Nothing]
                           ,[(Just 4), Nothing,  Nothing, Nothing]
                           ,[(Just 2), (Just 4), Nothing, Nothing]]
          shifted = shiftLeft a
      shifted `shouldBe` b
  describe "Shifting up" $ do
    it "Should take the a matrix and combine all similar numbers and shift them all to the left" $ do
      let a = M.fromLists  [[Nothing, (Just 2), (Just 2), (Just 2)]
                           ,[Nothing, (Just 2), (Just 2), Nothing]
                           ,[Nothing, Nothing, (Just 4), Nothing]
                           ,[Nothing, (Just 2), (Just 4), Nothing]]

          b = M.fromLists  [[Nothing, (Just 4), (Just 4), (Just 2)]
                           ,[Nothing, (Just 2), (Just 8), Nothing]
                           ,[Nothing, Nothing, Nothing, Nothing]
                           ,[Nothing, Nothing, Nothing, Nothing]]
          shifted = shiftUp a
      shifted `shouldBe` b
  describe "Shifting up" $ do
    it "Should take the a matrix and combine all similar numbers and shift them all to the left" $ do
      let a = M.fromLists  [[Nothing, (Just 2), (Just 2), (Just 2)]
                           ,[Nothing, (Just 2), (Just 2), Nothing]
                           ,[Nothing, Nothing, (Just 4), Nothing]
                           ,[Nothing, (Just 2), (Just 4), Nothing]]

          b = M.fromLists  [[Nothing, Nothing, Nothing, Nothing]
                           ,[Nothing, Nothing, Nothing, Nothing]
                           ,[Nothing, (Just 2), (Just 4), Nothing]
                           ,[Nothing, Just 4, (Just 8), (Just 2)]]
          shifted = shiftDown a
      shifted `shouldBe` b
  describe "Checking for moves" $ do
    it "Should not be able to modify the matrix" $ do
      let a = M.fromLists [[Just 2, Just 4, Just 8, Just 16]
                          ,[Just 4, Just 6, Just 7, Just 8]
                          ,[Just 9, Just 10, Just 11, Just 12]
                          ,[Just 13, Just 14, Just 15, Just 16]]
          b = possibleDirections a
      b `shouldBe` S.empty
    it "Should be able to move right and up" $ do
      let a = M.fromLists [[Just 2, Just 4, Just 8, Nothing]
                          ,[Just 4, Just 6, Just 7, Just 8]
                          ,[Just 9, Just 10, Just 11, Just 12]
                          ,[Just 13, Just 14, Just 15, Just 16]]
          b = possibleDirections a
      b `shouldBe`  S.fromList [MoveUp, MoveRight]
    it "Should be able to move left and up" $ do
      let a = M.fromLists [[Nothing, Just 4, Just 8, Just 16]
                          ,[Just 4, Just 6, Just 7, Just 8]
                          ,[Just 9, Just 10, Just 11, Just 12]
                          ,[Just 13, Just 14, Just 15, Just 16]]
          b = possibleDirections a
      b `shouldBe`  S.fromList [MoveUp, MoveLeft]
    it "Should be able to move right and down" $ do
      let a = M.fromLists [[Just 2, Just 4, Just 8, Just 16]
                          ,[Just 4, Just 6, Just 7, Just 8]
                          ,[Just 9, Just 10, Just 11, Just 12]
                          ,[Just 13, Just 14, Just 15, Nothing]]
          b = possibleDirections a
      b `shouldBe`  S.fromList [MoveDown, MoveRight]
    it "Should be able to move left and down" $ do
      let a = M.fromLists [[Just 2, Just 4, Just 8, Just 16]
                          ,[Just 4, Just 6, Just 7, Just 8]
                          ,[Just 9, Just 10, Just 11, Just 12]
                          ,[Nothing, Just 14, Just 15, Just 16]]
          b = possibleDirections a
      b `shouldBe` S.fromList [MoveDown, MoveLeft]
    it "Should be able to move all directions" $ do
      let a = M.fromLists [[Just 2, Just 4, Just 8, Just 16]
                          ,[Just 4, Nothing, Nothing, Just 8]
                          ,[Just 9, Nothing, Nothing, Just 12]
                          ,[Just 13, Just 14, Just 15, Just 16]]
          b = possibleDirections a
      b `shouldBe` S.fromList [MoveDown, MoveLeft, MoveUp, MoveRight]
    it "Should move down correctly" $ do
      let a = M.fromLists [ [ Just 4, Just 4, Just 8, Just 2]
                           ,[ Just 2, Just 4, Just 8, Just 32]
                           ,[ Just 8,  Just 16,  Just 32,  Just 64]
                           ,[ Just 16, Just 32, Just 64, Just 128]]
      let b = M.fromLists [ [ Just 4, Nothing, Nothing, Just 2]
                           ,[ Just 2, Just 8, Just 16, Just 32]
                           ,[ Just 8,  Just 16,  Just 32,  Just 64]
                           ,[ Just 16, Just 32, Just 64, Just 128]]
      (shiftDown a) `shouldBe` b
