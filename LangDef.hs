module LangDef where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Control.Parallel.Strategies (runEval)
import Data.Either
import Data.Functor.Identity
import Data.Maybe
import Text.Parsec.Char
import Text.Parsec (eof, parse, ParsecT, ParseError)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex
import Syntax
import System.Console.Haskeline

type Operator a = Ex.Operator String () Identity a

reservedNames' :: [String]
reservedNames' = []

reservedOps :: [String]
reservedOps = []

langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { Tok.commentStart = "{-"
  , Tok.commentEnd = "-}"
  , Tok.commentLine = "--"
  , Tok.nestedComments = True
  , Tok.identStart = letter
  , Tok.identLetter = alphaNum <|> oneOf "_'"
  , Tok.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedNames = reservedNames'
  , Tok.reservedOpNames = reservedOps
  , Tok.caseSensitive = True
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: (Parser a) -> (Parser a)
parens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

prefixOp :: String -> (a -> a) -> Operator a
prefixOp s f = Ex.Prefix (reservedOp s >> return f)

-- Prefix operators
table :: Ex.OperatorTable String () Identity Expr
table = [
    [
        prefixOp "succ" Succ
      , prefixOp "pred" Pred
      , prefixOp "iszero" IsZero
    ]
  ]

-- Constants
true :: Parser Expr
true = reserved "true" >> return Tr

false :: Parser Expr
false = reserved "false" >> return Fl

zero :: Parser Expr
zero = reservedOp "0" >> return Zero

factor :: Parser Expr
factor =
      true
  <|> false
  <|> zero
  <|> ifthen
  <|> parens expr

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

-- if/then/else
ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reservedOp "then"
  tr <- expr
  reserved "else"
  fl <- expr
  return (If cond tr fl)

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

isNum :: Expr -> Bool
isNum Zero = True
isNum (Succ t) = isNum t
isNum _ = False

isVal :: Expr -> Bool
isVal Tr = True
isVal Fl = True
isVal t | isNum t = True
isVal _ = False

eval' :: Expr -> Maybe Expr
eval' x = case x of
  IsZero Zero -> Just Tr
  IsZero (Succ t) | isNum t -> Just Fl
  IsZero t -> IsZero <$> (eval' t)
  Succ t -> Succ <$> (eval' t)
  Pred Zero -> Just Zero
  Pred (Succ t) | isNum t -> Just t
  Pred t -> Pred <$> (eval' t)
  If Tr c _ -> Just c
  If Fl _ a -> Just a
  If t c a -> (\t' -> If t' c a) <$> (eval' t)
  _ -> Nothing

nf :: Expr -> Expr
nf x = fromMaybe x (nf <$> eval' x)

eval :: Expr -> Maybe Expr
eval t = case nf t of
  nft | isVal nft -> Just nft
  otherwise -> Nothing

process :: String -> IO ()
process line = do
  let res = parseExpr line -- is this to avoid multiple runs?
  case res of
    Left err -> print err
    Right ex -> print $ eval ex

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "Repl> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> (liftIO $ process input) >> loop
