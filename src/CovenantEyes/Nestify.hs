{-# LANGUAGE FlexibleInstances #-}

module CovenantEyes.Nestify
  ( nestify
  ) where

import Data.Functor ((<$>))
import Data.List (foldl', isInfixOf)

import CovenantEyes.Nestify.Stack (Stack, emptyStack, pop, popWhile, push, size, toList)


class Nestifiable a where
  isBegin :: a -> Bool
  isEnd   :: a -> Bool
  scope   :: a -> String
  entry   :: a -> String
  build   :: a -> String -> a


instance Nestifiable [String] where
  isBegin xs = any (`isInfixOf` entry xs) ["Begin", "Enter"]
  isEnd   xs = any (`isInfixOf` entry xs) ["End",   "Exit"]
  scope = last        -- message is the last column
  entry = last . init -- scope name is the penultimate column

  build x y = x ++ [y]


(<++>) :: Nestifiable a => (Stack String, [a]) -> a -> (Stack String, [a])
(stack, result) <++> x = (newStack, thisLine : result)
  where
    isBegin' = isBegin x
    isEnd'   = isEnd x

    newStack
      | isBegin'  = scope x `push` stack
      | isEnd'    = pop (popWhile (/= scope x) stack)  -- jump over unbalanced scopes if needed
      | otherwise = stack

    thisLine
      | isEnd'    = indented (size newStack)
      | otherwise = indented (size stack)
      where indented d = build x (replicate d ' ' ++ entry x)


nestify :: Nestifiable a => [a] -> ([a], [String])
nestify = postProcess . foldl' (<++>) (emptyStack, [])
  where
    postProcess (stack, result) = (reverse result, toList stack)
