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

data Command
  = -- | print (1 + 1)
    Print Expression
  | -- | print 1; print 2
    Seq Command Command
  | -- | nop
    NoOp

evaluateExpression :: Expression -> Integer
evaluateExpression (Literal n) = n
evaluateExpression (Plus n m) = evaluateExpression n + evaluateExpression m
evaluateExpression (Minus n m) = evaluateExpression n - evaluateExpression m
evaluateExpression (Times n m) = evaluateExpression n * evaluateExpression m

evaluate :: Command -> IO ()
evaluate (Print n) = print (evaluateExpression n)
evaluate (Seq p q) = do
  evaluate p
  evaluate q
evaluate NoOp = return ()

example1 = Print (Literal 1 `Plus` Literal 1)

example2 = Print (Literal 4) `Seq` Print (Literal 2)

example3 = NoOp

main :: IO ()
main = do
  evaluate example1
  evaluate example2
  evaluate example3
