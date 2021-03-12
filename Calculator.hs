#!/usr/bin/env stack
{- stack
    ghci
    --resolver lts-17.4
 -}

module Main where

data AExp
  = Literal Integer
  | Plus AExp AExp
  | Minus AExp AExp
  | Times AExp AExp
  deriving (Show, Eq)

evaluateAExp :: AExp -> Integer
evaluateAExp (Literal n) = n
evaluateAExp (Plus n m) = evaluateAExp n + evaluateAExp m
evaluateAExp (Minus n m) = evaluateAExp n - evaluateAExp m
evaluateAExp (Times n m) = evaluateAExp n * evaluateAExp m

example1 = Literal 2

example2 = Plus (Literal 1) (Literal 1)

example3 = (Literal 1 `Plus` (Literal 2 `Times` Literal 2)) `Minus` Literal 3

main = do
  print (evaluateAExp example1)
  print (evaluateAExp example2)
  print (evaluateAExp example3)
