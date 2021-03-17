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
  | -- boolean operations consider numbers != 0 to be true and 0 to be false
    -- similarly to C coercion rules. This allows them to be used with IfNez
    -- easily / intuitively.
    -- we could easily add more like &&, ||, <=, >, <, !=
    Not Expression
  | Eq Expression Expression
  | Ge Expression Expression

data Command
  = Print Expression
  | Seq Command Command
  | NoOp
  | Assign String Expression
  | IfNez Expression Command Command

evaluateExpression :: Map String Integer -> Expression -> Integer
evaluateExpression env (Literal n) = n
evaluateExpression env (Variable x) =
  case Map.lookup x env of
    Just v -> v
    Nothing -> 0
evaluateExpression env (Plus n m) = evaluateExpression env n + evaluateExpression env m
evaluateExpression env (Minus n m) = evaluateExpression env n - evaluateExpression env m
evaluateExpression env (Times n m) = evaluateExpression env n * evaluateExpression env m
evaluateExpression env (Not n) =
  if evaluateExpression env n /= 0
    then 0
    else 1
evaluateExpression env (Eq n m) =
  if evaluateExpression env n == evaluateExpression env m
    then 1
    else 0
evaluateExpression env (Ge n m) =
  if evaluateExpression env n >= evaluateExpression env m
    then 1
    else 0

evaluate :: IORef (Map String Integer) -> Command -> IO ()
evaluate env (Print n) = do
  currentEnv <- readIORef env
  print (evaluateExpression currentEnv n)
evaluate env (Seq p q) = do
  evaluate env p
  evaluate env q
evaluate env NoOp = return ()
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
  Assign "x" (Literal 1) `Seq`
  IfNez (Variable "x" `Ge` Literal 3)
    (Print (Literal 0))
  -- else
    (
      IfNez (Variable "x" `Ge` Literal 2)
        (Print (Literal 0))
        (Print (Literal 42))
    )
{- ORMOLU_DISABLE -}

main :: IO ()
main = do
  run example1
