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

data Command
  = Print AExp
  | Seq Command Command
  | NoOp

evaluateAExp :: AExp -> Integer
evaluateAExp (Literal n) = n
evaluateAExp (Plus n m) = evaluateAExp n + evaluateAExp m
evaluateAExp (Minus n m) = evaluateAExp n - evaluateAExp m
evaluateAExp (Times n m) = evaluateAExp n * evaluateAExp m

evaluate :: Command -> IO ()
evaluate (Print n) = print (evaluateAExp n)
evaluate (Seq p q) = do
  evaluate p
  evaluate q
evaluate NoOp = pure ()

example1 = Print (Literal 1 `Plus` Literal 1)

example2 = Print (Literal 4) `Seq` Print (Literal 2)

example3 = NoOp

example4 = NoOp `Seq` NoOp

main :: IO ()
main = do
  evaluate example1
  evaluate example2
  evaluate example3
  evaluate example4
