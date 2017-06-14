module Parser (
  parseExpr
  , readExpr
  , tryParseExpr
  ) where

import Text.ParserCombinators.Parsec 
import Scheme 

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseString :: Parser Expr
parseString = do
                _ <- char '"'
                x <- many (noneOf "\"")
                _ <- char '"'
                return $ String x

parseNumber :: Parser Expr
parseNumber =  (Number . read) <$> many1 digit

  
parseAtom :: Parser Expr
parseAtom = do 
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of 
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom
 
parseList :: Parser Expr
parseList = do
     _ <- char '('
     es <- sepBy parseExpr spaces
     _ <- char ')'
     return $ List es

parsePair :: Parser Expr
parsePair = do
  _ <- char '('
  car <- parseExpr
  _ <- spaces
  _ <- char '.'
  _ <- spaces
  cdr <- parseExpr
  return $ Pair car cdr
  
parseExpr :: Parser Expr
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> (try parseList
              <|> parsePair)
        

readExpr :: String -> String
readExpr input = case parse (spaces >> parseExpr) "expression" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

tryParseExpr :: String -> Either String Expr
tryParseExpr input = case parse (spaces >> parseExpr) "expression" input of
  Left err -> Left $ "Failed to parse: " ++ (show err)
  Right e -> Right e
