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
  | -- boolean operations consider numbers != 0 to be true and 0 to be false
    -- similarly to C coercion rules. This allows them to be used with IfNez
    -- easily / intuitively.
    Not AExp
  | And AExp AExp
  | Or AExp AExp
  | Eq AExp AExp
  | Ge AExp AExp
  -- we could easily add more like <=, >, <, !=
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
evaluateAExp env (Not n) =
  if evaluateAExp env n /= 0
    then 0
    else 1
evaluateAExp env (And n m) =
  if evaluateAExp env n /= 0 && evaluateAExp env m /= 0
    then 1
    else 0
evaluateAExp env (Or n m) =
  if evaluateAExp env n /= 0 || evaluateAExp env m /= 0
    then 1
    else 0
evaluateAExp env (Eq n m) =
  if evaluateAExp env n == evaluateAExp env m
    then 1
    else 0
evaluateAExp env (Ge n m) =
  if evaluateAExp env n >= evaluateAExp env m
    then 1
    else 0

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
