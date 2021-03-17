#!/usr/bin/env stack
{- stack
    ghci
    --resolver lts-16.6
 -}

module Main where

data Expression
  = Literal Integer
  | Plus Expression Expression
  | Minus Expression Expression
  | Times Expression Expression

evaluateExpression :: Expression -> Integer
evaluateExpression (Literal n) = n
evaluateExpression (Plus n m) = evaluateExpression n + evaluateExpression m
evaluateExpression (Minus n m) = evaluateExpression n - evaluateExpression m
evaluateExpression (Times n m) = evaluateExpression n * evaluateExpression m

example1 = Literal 1 `Plus` (Literal 2 `Times` (Literal 3 `Plus` Literal 4))

example2 = Plus (Literal 1) (Times (Literal 2) (Plus (Literal 3) (Literal 4)))

main = do
  print (evaluateExpression example1)
  print (evaluateExpression example2)
