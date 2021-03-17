#!/usr/bin/env stack
{- stack
    ghci
    --resolver lts-17.4
    --package containers
 -}

module Main where

import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map

data AExp
  = Literal Integer
  | Variable String
  | Plus AExp AExp
  | Minus AExp AExp
  | Times AExp AExp

data Command
  = Print AExp
  | Seq Command Command
  | NoOp
  | -- | x := 1
    Assign String AExp

evaluateAExp :: Map String Integer -> AExp -> Integer
evaluateAExp env (Literal n) = n
evaluateAExp env (Variable x) =
  case Map.lookup x env of
    Just v -> v
    Nothing -> 0
evaluateAExp env (Plus n m) = evaluateAExp env n + evaluateAExp env m
evaluateAExp env (Minus n m) = evaluateAExp env n - evaluateAExp env m
evaluateAExp env (Times n m) = evaluateAExp env n * evaluateAExp env m

evaluate :: IORef (Map String Integer) -> Command -> IO ()
evaluate env (Print n) = do
  currentEnv <- readIORef env
  print (evaluateAExp currentEnv n)
evaluate env (Seq p q) = do
  evaluate env p
  evaluate env q
evaluate env NoOp = pure ()
evaluate env (Assign x n) = do
  currentEnv <- readIORef env
  -- env[x] = evaluateAExp currentEnv n
  writeIORef env (Map.insert x (evaluateAExp currentEnv n) currentEnv)

example1 = Print (Variable "x")

example2 = Assign "x" (Literal 2) `Seq` Print (Variable "x")

main :: IO ()
main = do
  run example1
  run example2

run :: Command -> IO ()
run c = do
  startingEnv <- newIORef Map.empty
  evaluate startingEnv c
