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

evaluateAExp :: AExp -> Integer
evaluateAExp (Literal n) = n
evaluateAExp (Plus n m) = evaluateAExp n + evaluateAExp m
evaluateAExp (Minus n m) = evaluateAExp n - evaluateAExp m
evaluateAExp (Times n m) = evaluateAExp n * evaluateAExp m

example1 = Literal 1 `Plus` (Literal 2 `Times` (Literal 3 `Plus` Literal 4))

example2 = Plus (Literal 1) (Minus (Literal 1) (Literal 1))

main = do
  print (evaluateAExp example1)
  print (evaluateAExp example2)
