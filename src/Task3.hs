{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

{-# LANGUAGE MultiParamTypeClasses #-}

module Task3 where

import Task1 (Parse, Parse(..))
import Task2 (Reify, Eval, Eval(..), evaluate)
import Data.List (nub)
import Data.Maybe (isNothing, fromMaybe)

-- | Solves SAT problem for given boolean formula written in Reverse Polish Notation
--
-- Returns whether given formula is satifiable
-- wrapped into 'Maybe' with 'Nothing' indicating parsing failure.
--
-- Only following binary operations are allowed:
-- - and
-- - or
-- - xor
--
-- All other words are considered as variables.
--

data BoolOp = And | Xor | Or
  deriving Show

instance Parse BoolOp where
  parse "and" = Just And
  parse "xor" = Just Xor
  parse "or"  = Just Or
  parse _     = Nothing

instance Eval Bool BoolOp where
  evalBinOp And a b = a && b
  evalBinOp Xor a b = a /= b
  evalBinOp Or  a b = a || b

-- Usage example:
--
-- >>> solveSAT "x y and"
-- Just True
-- >>> solveSAT "x y xor x y and and"
-- Just False
-- >>> solveSAT "x y xor x y and or"
-- Just True
-- >>> solveSAT "x xor"
-- Nothing
--
solveSAT :: String -> Maybe Bool
solveSAT str = 
    if any isNothing results then Nothing
    else Just satisfiable
    where 
        envs = createListOfLists str
        results = map (`evalLExpr` str) envs    
        boolResults = map (fromMaybe False) results
        satisfiable = or boolResults 


createListOfLists :: String -> [[(String, Bool)]]
createListOfLists str = mapM (\var -> [(var, False), (var, True)]) listUniqueWords
    where listUniqueWords = wordsNotInList ["and", "or", "xor"] (words str)
        

wordsNotInList :: [String] -> [String] -> [String]
wordsNotInList blacklist list =
  filter (`notElem` blacklist) (nub list)


evalLExpr :: [(String, Bool)] -> String -> Maybe Bool
evalLExpr = evaluate reifyBool

reifyBool :: Reify Bool BoolOp
reifyBool = id