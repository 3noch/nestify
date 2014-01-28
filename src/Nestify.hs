module Nestify
	( nestify
	) where

import Data.Functor ((<$>))
import Data.List (foldl', isInfixOf)

import Stack (Stack, emptyStack, pop, popWhile, push, size, toList)


(<++>) :: (Stack String, [[String]]) -> [String] -> (Stack String, [[String]])
(stack, result) <++> x = (newStack, thisLine : result)
  where
    isBegin = any (`isInfixOf` msg) ["Begin", "Enter"]
    isEnd   = any (`isInfixOf` msg) ["End",   "Exit"]

    msg   = last x           -- message is the last column
    scope = last . init $ x  -- scope name is the penultimate column

    newStack
      | isBegin   = scope `push` stack
      | isEnd     = pop (popWhile (/= scope) stack)  -- jump over unbalanced scopes if needed
      | otherwise = stack

    thisLine
      | isEnd     = indented (size newStack)
      | otherwise = indented (size stack)
      where indented d = init x ++ [replicate d ' ' ++ last x]


nestify :: [[String]] -> [[String]]
nestify = postProcess . foldl' (<++>) (emptyStack, [])
  where
    postProcess (stack, result) = reverse result ++ danglers
      where danglers = (\x -> ["-> Dangling: " ++ x]) <$> toList stack
