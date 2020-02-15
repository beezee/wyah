{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module NanoParsec where

import Data.Char
import Data.Functor
import Control.Applicative
import Control.Monad

newtype Parser a = Parser { parse :: String -> [(a, String)] }

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, rs)] -> error "Parser did not consume the stream"
    _ -> error "Parser error"

item :: Parser Char
item = Parser $ \s ->
  case s of
    [] -> []
    (c:cs) -> [(c, cs)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind pa apb = Parser $ \s -> 
  concatMap (\(a, s') -> parse (apb a) s') $ parse pa s

unit :: a -> Parser a
unit a = Parser (\s -> [(a, s)])

instance Functor Parser where
  fmap f pa = bind pa (unit . f)

instance Applicative Parser where
  pure = unit
  (Parser pab) <*> (Parser pa) = Parser $ (\s ->
    [(f a, s2) | (f, s1) <- pab s, (a, s2) <- pa s1])

instance Monad Parser where
  return = unit
  (>>=) = bind

failure :: Parser a
failure = Parser (\s -> [])

combine :: Parser a -> Parser a -> Parser a
combine (Parser p1) (Parser p2) = Parser $ \s -> (p1 s) ++ (p2 s)

option :: Parser a -> Parser a -> Parser a
option (Parser p1) (Parser p2) = Parser $ \s ->
  case p1 s of
    [] -> p2 s
    res -> res

instance Alternative Parser where
  empty = failure
  (<|>) = option

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

satisfy :: (Char -> Bool) -> Parser Char
satisfy cb = item >>= (\c ->
  case cb c of
    True -> unit c
    False -> empty)

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do { a <- p; rest a }
  where
    rest a = (do f <- op
                 b <- p
                 rest (f a b))
             <|> return a

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

char :: Char -> Parser Char
char c = satisfy (c ==)

natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

string :: String -> Parser String
string [] = return []
string (c:cs) = do { char c; string cs; return (c:cs) }

spaces :: Parser String
spaces = many $ oneOf "\n\r"

token :: Parser a -> Parser a
token p = do { a <- p; spaces; return a }

reserved :: String -> Parser String
reserved s = token (string s)

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Int
number = do
  s <- string "-" <|> return []
  cs <- some digit
  return $ read (s ++ cs)

parens :: Parser a -> Parser a
parens p = do
  reserved "("
  a <- p
  reserved ")"
  return a
