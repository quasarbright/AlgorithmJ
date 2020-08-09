module Parsing.ParseUtils where

import Control.Applicative hiding (some, many)
import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Debug as DBG

type Parser = Parsec Void String

dbg :: Show a => String -> Parser a -> Parser a
dbg desc p = (if False then DBG.dbg else (\ _ b -> b)) desc p <?> desc

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

blockComment :: Parser ()
blockComment = L.skipBlockCommentNested "{-" "-}"

-- | whitespace consumer that consumes newlines
scn :: Parser ()
scn = L.space space1 lineComment blockComment

-- | whitespace consumer that doesn't consume newlines
sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc -- NOT scn. IMPORTANT

symbol :: String -> Parser ()
symbol = void . L.symbol sc

pKeyword :: String -> Parser ()
pKeyword = pKeywordWith sc

pKeywordWith :: Parser () -> String -> Parser ()
pKeywordWith sc' word = void . L.lexeme sc' $ (string word <* notFollowedBy identLetter)

pReservedOp :: String -> Parser ()
pReservedOp = pReservedOpWith sc

pReservedOpWith :: Parser () -> String -> Parser ()
pReservedOpWith sc' name = void . L.lexeme sc' $ (string name <* notFollowedBy opLetter)

reservedWords :: [String]
reservedWords = words "data type newtype module import instance class as qualified do of let in case where fun when if then else"

reservedOps :: [String]
reservedOps = words "= -> => <- :: | ; .. \\ @ ~"

identStart :: Parser Char
identStart = letterChar <|> char '-'

identLetter :: Parser Char
identLetter = identStart <|> char '\'' <|> numberChar

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
 where
   p       = (:) <$> identStart <*> many identLetter
   check x =
     if x `elem` reservedWords
     then fail $ "keyword " ++ show x ++ " cannot be an identifier"
     else return x

tickedIdentifier :: Parser String
tickedIdentifier = between (symbol "`") (symbol "`") identifier

-- no opStart

opLetter :: Parser Char
opLetter = oneOf "\\|/-+=!@#$%^&*~.?<>"

operator :: Parser String
operator = operatorWith sc

operatorWith :: Parser () -> Parser String
operatorWith sc' = (L.lexeme sc' . try) (p >>= check) <|> L.lexeme sc' tickedIdentifier
   where
       p = some opLetter
       check x =
           if x `elem` reservedOps
           then fail $ "reserved operator " ++ show x ++ " cannot be an identifier"
           else return x

type SS = (SourcePos, SourcePos)

-- | run the given parser and record the source span of the parse
wrapSS :: Parser a -> Parser (a, SS)
wrapSS p = do
    startPos <- getSourcePos
    result <- p
    endPos <- getSourcePos
    return (result, (startPos, endPos))

wrapSSWith :: ((a, SS) -> b) -> Parser a -> Parser b
wrapSSWith f p = f <$> wrapSS p

combineSS :: (a1, b1) -> (a2, b2) -> (a1, b2)
combineSS a b = (fst a, snd b)

-- | optional with backtracking
vot :: Parser () -> Parser ()
vot = void . try . optional

-- | copied from the parsec package:
-- https://hackage.haskell.org/package/parsec-3.1.14.0/docs/src/Text.Parsec.Combinator.html#chainl1
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op        = do{ x <- p; rest x }
                    where
                      rest x    = do{ f <- op
                                    ; y <- p
                                    ; rest (f x y)
                                    }
                                <|> return x