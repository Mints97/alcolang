module Parser.Parser
where

import System.IO
import Control.Monad
import Text.Parsec hiding (try)
import Data.Char (isSpace)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

import Parser.Lexer

{-
f = (param1 = int) (param2 = string) (+ param1 (* 2 param2))
Grammar:
 ::= name = 
 ::= (  ) 
 ::= ( name list )
 ::= (  name  )

list ::=  list
list ::= 
-}

data UnparsedAlco = UnparsedAlco SourcePos String deriving (Eq, Show)

data ParsedAlco = BindToLiteral [ParsedAlco]
                | Pattern [UnparsedAlco]
    deriving (Eq, Show)


unparsedAlco :: Parser UnparsedAlco
unparsedAlco = UnparsedAlco <$> sourcePos <*> ((tail . init) <$> unparsedBlock)
           <|> UnparsedAlco <$> sourcePos <*> identifierName

alcoPattern :: Parser ParsedAlco
alcoPattern = do { af <- sepBy1 unparsedAlco whitespace ; whitespace ; return $ Pattern af }

alco :: Parser ParsedAlco
alco = try (do { af <- sepBy1 alcoPattern (do { whitespace ; s <- eqSign ; whitespace ; return s }) ; whitespace ; return $ BindToLiteral af })
   <|> try alcoPattern

parseString :: String -> ParsedAlco
parseString str = case parse (whitespace *> alco <* eof) "" str of
    Left e  -> error $ show e
    Right r -> r