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
  | Assign String Expression
  | -- if 1 then print 2 else print 3
    IfNez Expression Command Command

evaluateExpression :: Map String Integer -> Expression -> Integer
evaluateExpression env (Literal n) = n
evaluateExpression env (Variable x) =
  case Map.lookup x env of
    Just v -> v
    Nothing -> 0
evaluateExpression env (Plus n m) = evaluateExpression env n + evaluateExpression env m
evaluateExpression env (Minus n m) = evaluateExpression env n - evaluateExpression env m
evaluateExpression env (Times n m) = evaluateExpression env n * evaluateExpression env m

evaluate :: IORef (Map String Integer) -> Command -> IO ()
evaluate env (Print n) = do
  currentEnv <- readIORef env
  print (evaluateExpression currentEnv n)
evaluate env (Seq p q) = do
  evaluate env p
  evaluate env q
evaluate env NoOp = pure ()
evaluate env (Assign x n) = do
  currentEnv <- readIORef env
  writeIORef env (Map.insert x (evaluateExpression currentEnv n) currentEnv)
evaluate env (IfNez n p q) = do
  currentEnv <- readIORef env
  let result = evaluateExpression currentEnv n
  if result /= 0
    then evaluate env p
    else evaluate env q

run :: Command -> IO ()
run c = do
  startingEnv <- newIORef Map.empty
  evaluate startingEnv c

{- ORMOLU_DISABLE -}
example1 =
  IfNez (Variable "x")
    (Print (Literal 0))
  -- else
    (Print (Literal 42))

example2 =
  Assign "x" (Literal 1) `Seq`
  IfNez (Variable "x")
    (Print (Literal 42))
  -- else
    NoOp

main :: IO ()
main = do
  run example1
  run example2
