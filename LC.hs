module LC where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Parallel.Strategies (runEval)
import Data.Either
import Data.Functor.Identity
import Data.Maybe
import Text.Parsec (try)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec (eof, parse, ParsecT, ParseError)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex
import qualified Text.PrettyPrint as PP
import System.Console.Haskeline

type Name = String

data Lit
  = LInt Int
  | LBool Bool
  deriving (Show)

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  | Lit Lit
  deriving (Show)

subst :: Name -> Expr -> Expr -> Expr
subst n e2 e1 = case e1 of
  Var n1 | n == n1 -> e2
  Var n1 -> Var n1
  App e3 e4 -> app
    where
      e3' = subst n e2 e3
      e4' = subst n e2 e4
      app = App e3' e4'
  Lam n1 e3 | n == n1 -> Lam n1 e3
  Lam n1 e3 -> Lam n1 $ subst n e2 e3

eval :: Expr -> Maybe Expr
eval ex = case ex of
  App (Lam n e1) e2 -> return $ subst n e2 e1
  App e1 e2 -> (\x -> App x e2) <$> eval e1
  _ -> Nothing

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
    ]
  ]

notApp :: Parser Expr
notApp = 
      lambda
  <|> lit
  <|> var
  <|> parens expr

factor :: Parser Expr
factor =
      app <|> notApp

lit :: Parser Expr
lit =
      ((Lit . LInt . (\x -> (read x)::Int)) <$> (many1 $ digit))
  <|> Lit . LBool . const True <$> reserved "true"
  <|> Lit . LBool . const False <$> reserved "false"

var :: Parser Expr
var = do
  v <- many1 $ (alphaNum <|> oneOf "_'")
  Tok.whiteSpace lexer
  return $ Var v

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

app :: Parser Expr
app = try $ do
  e1 <- notApp
  Tok.whiteSpace lexer
  e2 <- notApp
  return $ App e1 e2

-- lambda
lambda :: Parser Expr
lambda = try $ do
  char '\\'
  Tok.whiteSpace lexer
  (Var name) <- var
  Tok.whiteSpace lexer
  reservedOp "->"
  Tok.whiteSpace lexer
  body <- expr
  Tok.whiteSpace lexer
  return (Lam name body)

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

nf :: Expr -> Expr
nf x = fromMaybe x (nf <$> eval x)

onf :: Expr -> Expr
onf x = fromMaybe x $ eval x

class Pretty p where
  ppr :: Int -> p -> PP.Doc

  pp :: p -> PP.Doc
  pp = ppr 0

viewVars :: Expr -> [Expr]
viewVars (Lam n a) = [Var n]
viewVars _ = []

viewBody :: Expr -> Expr
viewBody (Lam _ a) = viewBody a
viewBody x = x

parensIf :: Bool -> PP.Doc -> PP.Doc
parensIf True = PP.parens
parensIf _ = id

instance Pretty Expr where
  ppr p e = case e of
    Lit (LInt a) -> PP.text (show a)
    Lit (LBool b) -> PP.text (show b)
    Var x -> PP.text x
    App a b -> parensIf (p>0) $ (ppr (p+1) a) PP.<+> (ppr p b)
    Lam x a -> parensIf (p>0) $ 
           PP.char '\\' 
        PP.<> (PP.hsep $ pp <$> viewVars e)
        PP.<+> PP.text "->"
        PP.<+> ppr (p+1) (viewBody e)

ppexpr :: Expr -> String
ppexpr = PP.render . ppr 0

anf :: Expr -> [Expr]
anf x = fromMaybe [x] ((x:) . anf <$> eval x)

process :: String -> IO ()
process line = do
  let res = parseExpr line -- is this to avoid multiple runs?
  case res of
    Left err -> print err
    Right ex -> print $ ppexpr $ nf ex

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "Repl> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> (liftIO $ process input) >> loop
