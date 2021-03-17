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

evaluateExpr :: Map String Integer -> Expression -> Integer
evaluateExpr env (Literal n) = n
evaluateExpr env (Variable x) =
  case Map.lookup x env of
    Just v -> v
    Nothing -> 0
evaluateExpr env (Plus n m) = evaluateExpr env n + evaluateExpr env m
evaluateExpr env (Minus n m) = evaluateExpr env n - evaluateExpr env m
evaluateExpr env (Times n m) = evaluateExpr env n * evaluateExpr env m
evaluateExpr env (Not n) =
  if evaluateExpr env n /= 0
    then 0
    else 1
evaluateExpr env (Eq n m) =
  if evaluateExpr env n == evaluateExpr env m
    then 1
    else 0
evaluateExpr env (Ge n m) =
  if evaluateExpr env n >= evaluateExpr env m
    then 1
    else 0

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
