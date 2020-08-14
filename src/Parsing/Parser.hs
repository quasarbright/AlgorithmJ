{-# LANGUAGE TupleSections #-}

module Parsing.Parser where

import Control.Applicative hiding (some, many)
import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Debug.Trace

import Parsing.ParseUtils
import Parsing.ParseAst


--TODO maybe force indented case, let, and where and make simple expressions one-line

-- | makes its own line fold
pExpr :: Parser (Expr SS)
pExpr = dbg "expression (lf)" . L.lineFold scn $ \sc' -> topParser exprParsers sc'

pExprWith :: Parser () -> Parser (Expr SS)
pExprWith sc' = dbg "expression (no lf)" $ topParser exprParsers sc'

exprParsers :: [ExprParser]
exprParsers = [pCaseLetFun, pWhere, pIf, pOr, pAnnot, pBinop, pApp, pAtom]

pPatternExpr :: Parser (Expr SS)
pPatternExpr = dbg "pattern expr (lf)" . L.lineFold scn $ \sc' -> topParser patternParsers sc'

patternParsers :: [ExprParser]
patternParsers = [pAtom]-- [pOr, pAnnot, pBinop, pApp, pAtom]

topParser :: [(Parser a -> Parser b) -> Parser a -> Parser b] -> Parser a -> Parser b
topParser parsers = foldr (\ep p -> ep p) (error "you will never arrive at the truth") (cycle parsers)

--                child parser                      ws parser    output
type ExprParser = (Parser() -> Parser (Expr SS)) -> Parser () -> Parser (Expr SS)

-- | parses a case expression
-- case e of
--     p -> e
--     p -> e
--     ...
pCase :: ExprParser
pCase child sc' = dbg "case" $ do
    ((e, ms), ss) <- wrapSS . trace "case indent" (L.indentBlock scn) $ do
        pKeywordWith sc' "case"
        e <- child sc'
        pKeyword "of"
        return $ L.IndentSome Nothing (return . (e,)) pMatch
    return (Case e ms ss)

-- | parses p -> e
pMatch :: Parser (Expr SS, Expr SS)
pMatch = do
    lhs <- pPatternExpr -- need fresh line fold here
    pReservedOp "->" -- TODO line fold? hope not
    rhs <- pExpr -- need fresh line fold here
    return (lhs, rhs)

-- | let decls in e
pLet :: ExprParser
pLet child sc' = dbg "let" $ do
    ((decls, body), ss) <- wrapSS $ do
        decls <- indentBlock' scn $ do
            pKeywordWith sc' "let"
            return pDeclsHelp
        pKeywordWith sc' "in"
        body <- child sc'
        return (decls, body)
    return $ Let decls body ss

pFun :: ExprParser
pFun child sc' = dbg "fun" $ do
    ((args, body), ss) <- wrapSS $ do
        pKeywordWith sc' "fun"
        args <- child sc' -- TODO pPatternExprWith
        pReservedOpWith sc' "->"
        body <- child sc'
        return (args, body)
    return $ Fun args body ss

pCaseLetFun :: ExprParser
pCaseLetFun child sc' =
    let self = pCaseLetFun child in
    -- they can all contain each other unparenthesized
    choice [pCase self sc', pLet self sc', pFun self sc', child sc']

-- | e where decls
-- or just e
pWhere :: ExprParser
pWhere child sc' = dbg "expression with where" $ do
    startPos <- getSourcePos
    e <- child sc'
    mWhere <- optional (pWhereHelp sc')
    case mWhere of
        Nothing -> return e
        Just decls -> do
            endPos <- getSourcePos
            return $ Where e decls (startPos, endPos)

-- | where decls
pWhereHelp :: Parser () -> Parser [Decl SS]
pWhereHelp sc' = do
    indentBlock' scn $ do
        pKeywordWith sc' "where"
        return pDeclsHelp

-- | if e then e else e
pIf_ :: ExprParser
pIf_ child sc' = dbg "if" $ do
    ((cnd,thn,els), ss) <- wrapSS $ do
        pKeywordWith sc' "if"
        cnd <- child sc'
        pKeywordWith sc' "then"
        thn <- child sc'
        pKeywordWith sc' "else"
        els <- child sc'
        return (cnd,thn,els)
    return $ If cnd thn els ss

pIf :: ExprParser
pIf child sc' = pIf_ child sc' <|> child sc'

-- | e | e
pOr :: ExprParser
pOr child sc' = dbg "or" $ do
    (es, ss) <- wrapSS $ (child sc' <* vot sc') `sepBy1` pReservedOp "|" -- TODO <* necessary?
    return $ case es of
        [e] -> e
        _ -> Or es ss

-- | e :: t
pAnnot :: ExprParser
pAnnot child sc' = dbg "annot" $ do
    startPos <- getSourcePos
    e <- child sc'
    mType <- optional (pAnnotHelp sc')
    case mType of
        Nothing -> return e
        Just t -> do
            endPos <- getSourcePos
            return $ Annot e t (startPos, endPos)

-- | :: t
pAnnotHelp :: Parser () -> Parser (Type SS)
pAnnotHelp sc' = do
    pReservedOpWith sc' "::"
    pTypeWith sc'

-- | e op e (left associative, ignoring precedence)
pBinop :: ExprParser
pBinop child sc' = dbg "binop" $ chainl1 (child sc' <* vot sc') (binopChain sc')

binopChain :: Parser () -> Parser (Expr SS -> Expr SS -> Expr SS)
binopChain sc' = do
    name <- operatorWith sc'
    return (\ left right -> Binop left name defaultFixity right (combineSS (getTag left) (getTag right)))

-- | e e
pApp :: ExprParser
pApp child sc' = dbg "app" $ do
    (es, ss) <- wrapSS . some $ child sc'
    return $ case es of
        [e] -> e
        _ -> App es ss

-- | [e,e,...]
pList :: ExprParser
pList child sc' = dbg "list" $ do
    (es,ss) <- wrapSS $ do
        void $ L.symbol sc' "["
        es <- (child sc' <* vot sc') `sepBy` L.symbol sc' ","
        void $ L.symbol sc "]" -- NOTE use sc
        return es
    return $ List es ss

-- | (e,e,...)
pTup :: ExprParser
pTup child sc' =dbg "tuple" $ do
    (es,ss) <- wrapSS $ do
        void $ L.symbol sc' "("
        es <- (child sc' <* vot sc') `sepBy` L.symbol sc' ","
        void $ L.symbol sc ")" -- NOTE use sc
        return es
    return $ Tup es ss

-- | [...], (...), literal
pAtom :: ExprParser
pAtom top sc' = choice [try (pOpVar sc'), pList top sc', pTup top sc', pLiteral]

pOpVar :: Parser () -> Parser (Expr SS)
pOpVar sc' = dbg "op var" (wrapSSWith (uncurry OpVar) $ between (L.symbol sc' "(") (symbol ")") (operatorWith sc'))

pLiteral :: Parser (Expr SS)
pLiteral = choice [pWild, pNumber, pChar, pString]

pWild :: Parser (Expr SS)
pWild = dbg "wild" (wrapSSWith (Wild . snd) $ symbol "_")

pNumber :: Parser (Expr SS)
pNumber = dbg "number" $ lexeme (wrapSSWith (uncurry PDouble) (try L.float) <|> wrapSSWith (uncurry PInt) L.decimal)

pChar :: Parser (Expr SS)
pChar = dbg "char" (wrapSSWith (uncurry PChar) $ lexeme (between (char '\'') (char '\'') L.charLiteral))

pString :: Parser (Expr SS)
pString = dbg "string" (wrapSSWith (uncurry PString) $ lexeme (char '"' >> manyTill L.charLiteral (char '"')))


pType :: Parser (Type SS)
pType = dbg "type" $ fail "todo"

pTypeWith :: Parser () -> Parser (Type SS)
pTypeWith _ = dbg "type" $ fail "todo"

pDecl :: Parser (Decl SS)
pDecl = dbg "decl" $ fail "todo"

pDeclsHelp :: L.IndentOpt Parser [Decl SS] (Decl SS)
pDeclsHelp = L.IndentSome Nothing return pDecl