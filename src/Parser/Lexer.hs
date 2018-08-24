module Parser.Lexer (eqSign, tilde, unparsedBlock, identifierName, sourcePos, whitespace)
where

import System.IO
import Control.Monad
import Text.Parsec hiding (try)
import Data.Char (isSpace)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

data ParenHolder = NonParenTerm String
                 | ParenBlock [ParenHolder]
    deriving (Show)

parenAnythingContainer :: Char -> Char -> Parser ParenHolder
parenAnythingContainer po pc = ParenBlock <$> between (char po) (char pc) (many $ (try $ parenAnythingContainer po pc) <|> (NonParenTerm <$> nonParenBlock))
    where nonParenBlock = many1 $ noneOf [po, pc]

parenAnythingToStr :: Char -> Char -> ParenHolder -> String
parenAnythingToStr _ _ (NonParenTerm x) = x
parenAnythingToStr po pc (ParenBlock xs) = po : (parenAnythingToStr po pc =<< xs) ++ [pc]


sourcePos :: Monad m => ParsecT s u m SourcePos
sourcePos = statePos `liftM` getParserState


parenAnything :: Char -> Char -> Parser String
parenAnything po pc = parenAnythingToStr po pc <$> parenAnythingContainer po pc

eqSign :: Parser String
eqSign = string "="

tilde :: Parser String
tilde = string "~"

unparsedBlock :: Parser String
unparsedBlock = parenAnything '(' ')'

strName :: Parser String
strName = parenAnything '{' '}'

numName :: Parser String
numName = many1 digit

opName :: Parser String
opName = foldr1 (<|>) $ map string ["+", "*", "-", "/", "%", "==", "~", "!"]

many1Till :: (Stream s m t, Show end) => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
many1Till p end = do
    notFollowedBy end
    p1 <- p
    ps <- manyTill p end
    return (p1:ps)

identifierName :: Parser String
identifierName = opName <|> (many1 alphaNum) --many1Till anyChar (lookAhead (opName <|> strName <|> unparsedBlock <|> eqSign <|> (many1 space)))

anyName :: Foldable a => a (Parser String) -> Parser String
anyName = foldl1 (<|>)

whitespace :: Parser String
whitespace = many space

--Token :: Parser Token
--Token = EqSign <$> eqSign
--        <|> UnparsedData <$> sourcePos <*> ((tail . init) <$> unparsedBlock)
--        <|> Name <$> Name

--Tokens :: Parser [Token]
--Tokens = (many1 $ try ((many space) *> Token)) <* (many space) <* eof