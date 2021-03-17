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
  writeIORef env (Map.insert x (evaluateExpr currentEnv n) currentEnv)
evaluate env (IfNez n p q) = do
  currentEnv <- readIORef env
  let result = evaluateExpr currentEnv n
  if result /= 0
    then evaluate env p
    else evaluate env q

run :: Command -> IO ()
run c = do
  startingEnv <- newIORef Map.empty
  evaluate startingEnv c

{- ORMOLU_DISABLE -}
example1 =
  IfNez (Literal 1)
    (Print (Literal 42))
  -- else
    (Print (Literal 0))

example2 =
  Assign "x" (Literal 1) `Seq`
  IfNez (Variable "x")
    NoOp
  -- else
    (Print (Literal 666))

main :: IO ()
main = do
  run example1
  run example2
