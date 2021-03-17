#!/usr/bin/env stack
{- stack
    ghci
    --resolver lts-16.6
 -}

module Main where

data AExp
  = Literal Integer
  | Plus AExp AExp
  | Minus AExp AExp
  | Times AExp AExp

data Command
  = -- | print (1 + 1)
    Print AExp
  | -- | print 1; print 2
    Seq Command Command
  | -- | nop
    NoOp

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

main :: IO ()
main = do
  evaluate example1
  evaluate example2
  evaluate example3
