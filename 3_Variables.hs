#!/usr/bin/env stack
{- stack
    ghci
    --resolver lts-16.6
    --package containers
 -}

module Main where

import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map

data Expression
  = Literal Integer
  | Variable String
  | Plus Expression Expression
  | Minus Expression Expression
  | Times Expression Expression

data Command
  = Print Expression
  | Seq Command Command
  | NoOp
  | -- | x := 1
    Assign String Expression

evaluateExpr :: Map String Integer -> Expression -> Integer
evaluateExpr env (Literal n) = n
evaluateExpr env (Variable x) =
  case Map.lookup x env of
    Just v -> v
    Nothing -> 0
evaluateExpr env (Plus n m) = evaluateExpr env n + evaluateExpr env m
evaluateExpr env (Minus n m) = evaluateExpr env n - evaluateExpr env m
evaluateExpr env (Times n m) = evaluateExpr env n * evaluateExpr env m

evaluate :: IORef (Map String Integer) -> Command -> IO ()
evaluate env (Print n) = do
  currentEnv <- readIORef env
  print (evaluateExpr currentEnv n)
evaluate env (Seq p q) = do
  evaluate env p
  evaluate env q
evaluate env NoOp = return ()
evaluate env (Assign x n) = do
  currentEnv <- readIORef env
  -- env[x] = evaluateExpr currentEnv n
  writeIORef env (Map.insert x (evaluateExpr currentEnv n) currentEnv)

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
