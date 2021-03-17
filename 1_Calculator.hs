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

evaluateExpr :: Expression -> Integer
evaluateExpr (Literal n) = n
evaluateExpr (Plus n m) = evaluateExpr n + evaluateExpr m
evaluateExpr (Minus n m) = evaluateExpr n - evaluateExpr m
evaluateExpr (Times n m) = evaluateExpr n * evaluateExpr m

example1 = Literal 1 `Plus` (Literal 2 `Times` (Literal 3 `Plus` Literal 4))

example2 = Plus (Literal 1) (Times (Literal 2) (Plus (Literal 3) (Literal 4)))

main = do
  print (evaluateExpr example1)
  print (evaluateExpr example2)
