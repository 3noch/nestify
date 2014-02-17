module CovenantEyes.Nestify.Stack
  ( Stack(..)
  , emptyStack
  , push
  , pop
  , top
  , size
  , popWhile
  , fromList
  , toList
  ) where

data Stack a = Stack { _stack :: [a], _depth :: Int }

instance Show a => Show (Stack a) where
  show (Stack list _) = show list

instance Eq a => Eq (Stack a) where
  (Stack s1 d1) == (Stack s2 d2) = d1 == d2 && s1 == s2

emptyStack :: Stack a
emptyStack = Stack [] 0

push :: a -> Stack a -> Stack a
a `push` (Stack list depth) = Stack (a:list) (depth+1)

pop :: Stack a -> Stack a
pop (Stack []     _)     = Stack []  0
pop (Stack (_:xs) depth) = Stack xs (depth - 1)

top :: Stack a -> Maybe a
top (Stack [] _)    = Nothing
top (Stack (x:_) _) = Just x

size :: Stack a -> Int
size (Stack _ s) = s

popWhile :: (a -> Bool) -> Stack a -> Stack a
popWhile _ (Stack [] _) = emptyStack
popWhile p stack@(Stack (x:xs) depth)
  | p x       = popWhile p (Stack xs (depth-1))
  | otherwise = stack

fromList :: [a] -> Stack a
fromList as = Stack as (length as)

toList :: Stack a -> [a]
toList (Stack as _) = as
