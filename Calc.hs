module Main where

import Control.Applicative
import Control.Monad
import Data.Functor
import NanoParsec

data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Lit Int
  deriving Show

eval :: Expr -> Int
eval e = case e of
  Add e1 e2 -> (eval e1) + (eval e2)
  Mul e1 e2 -> (eval e1) * (eval e2)
  Sub e1 e2 -> (eval e1) - (eval e2)
  Lit i -> i

int :: Parser Expr
int = Lit <$> number

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp s f = reserved s >> return f

addOp :: Parser (Expr -> Expr -> Expr)
addOp = (infixOp "+" Add) <|> (infixOp "-" Sub)

mulOp :: Parser (Expr -> Expr -> Expr)
mulOp = infixOp "*" Mul

expr :: Parser Expr
expr = term `chainl1` addOp

term :: Parser Expr
term = factor `chainl1` mulOp

factor :: Parser Expr
factor = int <|> parens expr

run :: String -> Expr
run = runParser expr

main :: IO ()
main = forever $ do
    putStr "> "
    a <- getLine
    print $ eval $ run a
