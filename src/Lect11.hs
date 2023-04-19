-- CS 340: Programming Paradigms and Patterns
-- Lect 11 - Monadic Parsing
-- Michael Lee

module Lect11 where

import Data.Char
import Prelude hiding (fail)

data State s a = State {runState :: s -> Maybe (s, a)}

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f st = State $ \s ->
    case runState st s of
      Nothing -> Nothing
      Just (s', x) -> Just (s', f x)

instance Applicative (State s) where
  pure :: a -> State s a
  pure x = State $ \s -> Just (s, x)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  stf <*> stx = State $ \s ->
    case runState stf s of
      Nothing -> Nothing
      Just (s', f) -> runState (f <$> stx) s'

instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  st >>= f = State $ \s ->
    case runState st s of
      Nothing -> Nothing
      Just (s', x) -> runState (f x) s'

type Parser a = State String a

-- Parser which pops the first character off of a string
item :: Parser Char
item = State $ \s -> case s of
  "" -> Nothing
  (c : cs) -> Just (cs, c)

-- Parser guaranteed to fail
fail :: Parser a
fail = State (\_ -> Nothing)

-- Parser which takes a predicate
-- and returns if the first character of the string satisfies the predicate
sat :: (Char -> Bool) -> Parser Char
sat p = do
  c <- item -- First line of do block will be given the input state
  if p c then return c else fail

-- Parser which returns if the first character of the string is the given character
char :: Char -> Parser Char
char c = sat (== c)

--
string :: String -> Parser String
string "" = return ""
string s@(x : xs) = do
  char x
  string xs
  return s

-- Problem: using the state monad, write a parser that attempts to parse and
--          evaluate an infix arithmetic expression

-- Example: "1 + 2 * 3" -> 7
--          "1 + 2 * 3 + 4" -> 11
--          "(1 + 2) * (3 + 4))" -> 21

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr
  deriving (Show)

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e f) = eval e + eval f
eval (Mul e f) = eval e * eval f

digit :: Parser Char
digit = sat isDigit

digits :: Parser [Char]
digits = do
  d <- digit
  ds <- digits <|> return ""
  return (d : ds)

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = do
  x <- p
  xs <- oneOrMore p <|> return []
  return (x : xs)

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> return []

-- Takes 2 parses and returns the first of the 2 parsers that succeeds
pOr :: Parser a -> Parser a -> Parser a
p `pOr` q = State $ \s -> case runState p s of
  Nothing -> runState q s
  Just x -> Just x

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = pOr

int :: Parser Int
int = do
  ds <- digits
  return $ read ds

space :: Parser ()
space = do
  zeroOrMore (sat isSpace)
  return ()

token :: Parser a -> Parser a
token p = do
  space
  x <- p
  space
  return x

symbol :: String -> Parser String
symbol s = token (string s)

-- Grammar (in Backus-Naur Form) for infix arithmetic expressions:
--
--   expr   ::= term + expression | term
--   term   ::= factor * term | factor
--   factor ::= ( expr ) | integer

expr :: Parser Expr
expr =
  do
    t <- term
    symbol "+"
    e <- expr
    return $ Add t e
    <|> term

term :: Parser Expr
term =
  do
    f <- factor
    symbol "*"
    t <- term
    return $ Mul f t
    <|> factor

factor :: Parser Expr
factor =
  do
    symbol "("
    e <- expr
    symbol ")"
    return e
    <|> do
      n <- token int
      return $ Lit n

parseEval :: String -> Either String Int
parseEval s = case runState expr s of
  Nothing -> Left "No parse"
  Just ("", e) -> Right $ eval e
  _ -> Left "Incomplete parse"
