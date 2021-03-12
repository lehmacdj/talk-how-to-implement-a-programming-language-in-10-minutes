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
import Data.Maybe (fromJust)

data AExp
  = Literal Integer
  | Variable String
  | Plus AExp AExp
  | Minus AExp AExp
  | Times AExp AExp
  deriving (Show, Eq)

data Command
  = Print AExp
  | Seq Command Command
  | NoOp
  | Assign String AExp
  | IfNez AExp Command Command

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
  writeIORef env (Map.insert x (evaluateAExp currentEnv n) currentEnv)
evaluate env (IfNez n p q) = do
  currentEnv <- readIORef env
  let result = evaluateAExp currentEnv n
  if result == 0
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
  Assign "x" (Literal 0) `Seq`
  IfNez (Variable "x")
    (Print (Literal 42))
  -- else
    NoOp

example3 =
  Assign "x" (Literal 2) `Seq`
  IfNez (Variable "x")
    (Print (Literal 42))
  -- else
    NoOp `Seq`
  Print (Variable "x")
{- ORMOLU_ENABLE -}

main :: IO ()
main = do
  run example1
  run example2
  run example3
