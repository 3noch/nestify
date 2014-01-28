{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Hspec
import Test.QuickCheck

import Stack


instance Arbitrary a => Arbitrary (Stack a) where
  arbitrary = do
    list <- arbitrary
    return $ Stack list (length list)

emptyIntStack :: Stack Int
emptyIntStack = emptyStack

main :: IO ()
main = hspec $ do
  describe "emptyStack" $ do
    it "gives an empty stack" $ do
      toList emptyIntStack `shouldBe` []
      size emptyIntStack   `shouldBe` 0

  describe "size" $ do
    it "gives the size of the stack" $ property $
      \(x :: Stack Int) -> size x == length (toList x)

  describe "push" $ do
    it "adds an item to the top of the stack" $ property $
      \(x :: Stack Int) -> size (1 `push` x) == size x + 1

  describe "top" $ do
    it "gives Nothing on an empty stack" $ do
      top emptyIntStack `shouldBe` Nothing

    it "gives the top item on a stack" $ property $
      \(x :: Stack Int) -> top (1 `push` x) == Just 1

  describe "pop" $ do
    it "does nothing to an empty stack" $ do
      pop emptyIntStack `shouldBe` emptyIntStack

    it "removes the top item on a stack" $ property $
      \(x :: Stack Int) -> size x > 0 ==> tail (toList x) == toList (pop x)

  describe "popWhile" $ do
    it "does nothing on an empty stack" $ do
      popWhile (const True) emptyIntStack `shouldBe` emptyIntStack

    it "does nothing when the predicate always fails" $ property $
      \(x :: Stack Int) -> popWhile (const False) x == x

    it "gives an empty stack when the predicate always succeeds" $ property $
      \(x :: Stack Int) -> popWhile (const True) x == emptyIntStack

    it "gives a stack without it's top when the top matches the predicate" $ do
      popWhile (== 1) (fromList [1])    `shouldBe` emptyIntStack
      popWhile (== 1) (fromList [1, 1]) `shouldBe` emptyIntStack
      popWhile (== 1) (fromList [1, 2]) `shouldBe` fromList [2]
      popWhile (== 1) (fromList [1, 2, 1]) `shouldBe` fromList [2, 1]
