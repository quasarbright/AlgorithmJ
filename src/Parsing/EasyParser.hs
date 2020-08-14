module Parsing.EasyParser where

import Control.Applicative hiding (some, many)
import Control.Monad (void)
--import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
--import Debug.Trace
import Syntax.Names

import Parsing.ParseUtils
import Parsing.ParseAst
import Data.Functor (($>))

{-
whitespace insensitive. C-like syntax with braces and semicolons
-}

lexeme' :: Parser a -> Parser a
lexeme' = L.lexeme scn

symbol' :: String -> Parser String
symbol' = L.symbol scn

braces :: Parser a -> Parser a
braces = between (symbol' "{") (symbol' "}")

brackets :: Parser a -> Parser a
brackets = between (symbol' "[") (symbol' "]")

parens :: Parser a -> Parser a
parens = between (symbol' "(") (symbol' ")")

beginEnd :: Parser a -> Parser a
beginEnd = between (pKeyword' "begin") (pKeyword' "end")

-- | {p;p;...p;}
block :: Parser a -> Parser [a]
block p = braces (some $ p <* symbol' ";")

-- | {p;p;...p;} or just p;
blockOrSingle :: Parser a -> Parser [a]
blockOrSingle p = block p <|> ((: []) <$> p <* symbol' ";")

pKeyword' :: String -> Parser ()
pKeyword' = pKeywordWith scn

pReservedOp' :: String -> Parser ()
pReservedOp' = pReservedOpWith scn

operator' :: Parser String
operator' = operatorWith scn

identifier' :: Parser String
identifier' = identifier <* scn

type ExprParser = Parser (Expr SS) -> Parser (Expr SS)

topParser :: [Parser a -> Parser a] -> Parser a
topParser parsers = foldr (\ep p -> ep p) (error "you will never arrive at the truth") (cycle parsers)


-- expressions


pExpr :: Parser (Expr SS)
pExpr = dbg "expression" $ topParser [pCaseLetFun, pWhere, pIf, pAnnot, pBinop, pApp, pAtom]

pPattern :: Parser (Expr SS)
pPattern = dbg "pattern" $ topParser [pBinop, pApp, pAtom]

-- | case e of | p -> e | p -> e ...
pCase :: Parser (Expr SS)
pCase = dbg "case" $ do
    wrapSSWith (uncurry (uncurry Case)) $ do
        pKeyword' "case"
        e <- pExpr
        pKeyword' "of"
        ms <- (void . optional) (pReservedOp' "|") *> sepBy pMatch (pReservedOp' "|")
        return (e, ms)

pMatch :: Parser (Expr SS, Expr SS)
pMatch = dbg "match" $ do
    lhs <- pPattern
    pReservedOp' "->"
    rhs <- pExpr
    return (lhs, rhs)

pLet :: Parser (Expr SS)
pLet = dbg "let" . wrapSSApp $ do
    pKeyword' "let"
    Let <$> (pDecls <* pKeyword' "in") <*> pExpr

pFun :: Parser (Expr SS)
pFun = dbg "fun" . wrapSSApp $ do
    pKeyword' "fun"
    Fun <$> (pPattern <* pReservedOp' "->") <*> pExpr

pCaseLetFun :: ExprParser
pCaseLetFun child = choice [pCase, pLet, pFun, child]

pWhere :: ExprParser
pWhere child = dbg "where" $ do
    startPos <- getSourcePos
    e <- child
    mDecls <- optional (pKeyword' "where" *> pDecls)
    case mDecls of
        Nothing -> return e
        Just decls -> do
            endPos <- getSourcePos
            return $ Where e decls (startPos, endPos)

pIf :: ExprParser
pIf child = (dbg "if" . wrapSSApp $ If <$> go "if" <*> go "then" <*> go "else") <|> child
    where go word = pKeyword' word *> child

-- TODO make it a chainl like binop. should be easy, but you'll need to change the ast
pOr :: ExprParser
pOr child = dbg "or" . wrapSSApp $ do
    es <- child `sepBy1` pReservedOp' "|"
    return $ case es of
        [e] -> const e
        _ -> Or es

pAnnot :: ExprParser
pAnnot child = dbg "annot" . wrapSSApp $ do
    e <- child
    mType <- optional (pReservedOp' "::" *> pType)
    return $ case mType of
        Nothing -> const e
        Just t -> Annot e t

pBinop :: ExprParser
pBinop child = dbg "binop" $ chainl1 child binopChain


binopChain :: Parser (Expr SS -> Expr SS -> Expr SS)
binopChain = do
    name <- operator'
    return (\ left right -> Binop left name defaultFixity right (combineSS (getTag left) (getTag right)))

pApp :: ExprParser
pApp child = dbg "application" . wrapSSApp $ do
    es <- dbg "children" $ some child
    return $ case es of
        [e] -> const e
        _ -> App es

pList :: ExprParser
pList child = dbg "list" . wrapSSApp $ List <$> brackets (child `sepBy` pReservedOp' ",")

pTuple :: ExprParser
pTuple child = dbg "tuple" . wrapSSApp $ Tup <$> parens (child `sepBy` pReservedOp' ",")

pBeginEnd :: ExprParser
pBeginEnd child = dbg "begin end" . wrapSSApp $ BeginEnd <$> beginEnd child

pAtom :: ExprParser
pAtom child = choice [try pOpVar, pLiteral, pTuple child, pList child, pBeginEnd child]

pOpVar :: Parser (Expr SS)
pOpVar = dbg "op var" . wrapSSApp $ OpVar <$> parens operator'

pLiteral :: Parser (Expr SS)
pLiteral = dbg "literal" $ choice [pChar, pString, try pDouble, pInt, pVar, pWild]

pChar :: Parser (Expr SS)
pChar = dbg "char" . wrapSSApp $ PChar <$> lexeme' (between (char '\'') (char '\'') L.charLiteral)

pString :: Parser (Expr SS)
pString = dbg "string" . wrapSSApp $ PString <$> lexeme' (between (char '"') (char '"') (many L.charLiteral))

pDouble :: Parser (Expr SS)
pDouble = dbg "float" . wrapSSApp $ PDouble <$> lexeme' L.float

pInt :: Parser (Expr SS)
pInt = dbg "int" . wrapSSApp $ PInt <$> lexeme' L.decimal

pVar :: Parser (Expr SS)
pVar = dbg "variable" . wrapSSApp $ Var <$> identifier'

pWild :: Parser (Expr SS)
pWild = dbg "wildcard pattern" $ wrapSSWith (Wild . snd) (pKeyword' "_")


-- decls


pDecls :: Parser [Decl SS]
pDecls = blockOrSingle pDecl

pDecl :: Parser (Decl SS)
pDecl = dbg "declarations" $ choice [pDataDecl, pFixityDecl, try pOpAnnotDecl, try pAnnotDecl, pBindingDecl]

pOpAnnotDecl :: Parser (Decl SS)
pOpAnnotDecl = dbg "operator type decl" . wrapSSApp $ OpAnnotDecl <$> (parens operator' <* pReservedOp' "::") <*> pType

pAnnotDecl :: Parser (Decl SS)
pAnnotDecl = dbg "variable type decl" . wrapSSApp $ AnnotDecl <$> (identifier' <* pReservedOp' "::") <*> pType

pBindingDecl :: Parser (Decl SS)
pBindingDecl = dbg "binding decl" . wrapSSApp $ Binding <$> (pPattern <* pReservedOp' "=") <*> pExpr

pDataDecl :: Parser (Decl SS)
pDataDecl = dbg "data definition" . wrapSSApp $ do
    pKeyword' "data"
    name <- identifier'
    params <- many identifier'
    pReservedOp' "="
    cases <- pType `sepBy1` pReservedOp' "|"
    return $ DataDecl name params cases

pFixityDecl :: Parser (Decl SS)
pFixityDecl = dbg "fixity" . wrapSSApp $ do
    assoc <- pAssoc
    prec <- lexeme' L.decimal
    FixityDecl (MkFixity assoc prec) <$> operator'

pAssoc :: Parser Assoc
pAssoc = dbg "associativity" $ choice
    [ pKeyword' "infixl" $> LAssoc
    , pKeyword' "infixr" $> RAssoc
    , pKeyword' "infix"  $> NonAssoc
    ]


-- types


type TypeParser = Parser (Type SS) -> Parser (Type SS)

pType :: Parser (Type SS)
pType = dbg "type" $ topParser [pTArr, pTBinop, pTApp, pTAtom]

pTArr :: TypeParser
pTArr child = dbg "function type" $ chainr1 child arrChain

arrChain :: Parser (Type SS -> Type SS -> Type SS)
arrChain = pReservedOp' "->" $> (\ left right -> TArr left right (combineSS (getTypeTag left) (getTypeTag right)))

pTBinop :: TypeParser
pTBinop child = dbg "type binop" $ chainl1 child tBinopChain

pTApp :: TypeParser
pTApp child = dbg "type application" . wrapSSApp $ do
    ts <- some child
    return $ case ts of
        [t] -> const t
        _ -> TApp ts

tBinopChain :: Parser (Type SS -> Type SS -> Type SS)
tBinopChain = do
    name <- operator'
    return (\ left right -> TBinop left name defaultFixity right (combineSS (getTypeTag left) (getTypeTag right)))

pTAtom :: TypeParser
pTAtom child = choice [try pTOpVar, try pTJustList, pTTup child, pTList child, pTVar]

pTTup :: TypeParser
pTTup child = dbg "tuple type" . wrapSSApp $ TTup <$> parens (child `sepBy` pReservedOp' ",")

pTList :: TypeParser
pTList child = dbg "list type" . wrapSSApp $ TList <$> brackets child

pTOpVar :: Parser (Type SS)
pTOpVar = dbg "type opvar" . wrapSSApp $ TOpVar <$> parens operator'

pTJustList :: Parser (Type SS)
pTJustList = wrapSSWith (TJustList . snd) (symbol' "[]")

pTVar :: Parser (Type SS)
pTVar = dbg "type variable" . wrapSSApp $ TVar <$> identifier'


-- program


pProgram :: Parser (Program SS)
pProgram = scn *> wrapSSApp (Program <$> many (pDecl <* pReservedOp' ";")) <* eof

