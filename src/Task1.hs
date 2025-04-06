{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task1 where

-- * Expression data type

-- | Representation of integer arithmetic expressions comprising
-- - Literals of type 'a'
-- - Binary operations 'Add' and 'Mul'
data IExpr =
    Lit Integer
  | Add IExpr IExpr
  | Mul IExpr IExpr
  deriving Show

-- * Evaluation

-- | Evaluates given 'IExpr'
--
-- Usage example:
--
-- >>> evalIExpr (Lit 2)
-- 2
-- >>> evalIExpr (Add (Lit 2) (Lit 3))
-- 5
-- >>> evalIExpr (Add (Mul (Lit 3) (Lit 2)) (Lit 3))
-- 9
--
evalIExpr :: IExpr -> Integer
evalIExpr (Lit a)   = a
evalIExpr (Add a b) = evalIExpr a + evalIExpr b
evalIExpr (Mul a b) = evalIExpr a * evalIExpr b

-- * Parsing

-- | Class of parseable types
class Parse a where
  -- | Parses value 'a' from given string
  -- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
  parse :: String -> Maybe a

instance Parse Integer where
  parse x = if num x then Just(read x) else Nothing

instance Parse Bool where
  parse "1" = Just True
  parse "0" = Just False
  parse _ = Nothing

-- | Parses given expression in Reverse Polish Notation
-- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
--
-- Usage example:
--
-- >>> parse "2" :: Maybe IExpr
-- Just (Lit 2)
-- >>> parse "2 3 +" :: Maybe IExpr
-- Just (Add (Lit 2) (Lit 3))
-- >>> parse "3 2 * 3 +" :: Maybe IExpr
-- Just (Add (Mul (Lit 3) (Lit 2)) (Lit 3))
-- >>> parse "2 +" :: Maybe IExpr
-- Nothing
-- >>> parse "2 3" :: Maybe IExpr
-- Nothing
--
instance Parse IExpr where
  parse [] = Nothing
  parse list = parseR (words list) []

parseR :: [String] -> [IExpr] -> Maybe IExpr
parseR [] [expr] = Just expr
parseR [] _ = Nothing
parseR (x : xs) stack = if num x then parseR xs (Lit (read x) : stack) else 
  case x of
    "+" -> case stack of
      (a : b : bs) -> parseR xs (Add b a : bs)
      _ -> Nothing
    "*" -> case stack of
      (a : b : bs) -> parseR xs (Mul b a : bs)
      _ -> Nothing
    _ -> Nothing


num :: String -> Bool
num = all isDigit 

isDigit :: Char -> Bool
isDigit c = c `elem` ['0'..'9']

-- * Evaluation with parsing

-- | Parses given expression in Reverse Polish Notation and evaluates it
--
-- Returns 'Nothing' in case the expression could not be parsed.
--
-- Usage example:
--
-- >>> evaluateIExpr "2"
-- Just 2
-- >>> evaluateIExpr "2 3 +"
-- Just 5
-- >>> evaluateIExpr "3 2 * 3 +"
-- Just 9
-- >>> evaluateIExpr "2 +"
-- Nothing
-- >>> evaluateIExpr "2 3"
-- Nothing
--
evaluateIExpr :: String -> Maybe Integer
evaluateIExpr str = case parse str of
  Just a  -> Just (evalIExpr a)
  Nothing -> Nothing  