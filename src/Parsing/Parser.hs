{-# LANGUAGE TupleSections #-}

module Parsing.Parser where

import Control.Applicative hiding (some, many)
import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Parsing.ParseUtils
import Parsing.ParseAst

pExpr = undefined

pPatternExpr = undefined
--pExpr :: Parser (Expr SS)
--pExpr = dbg "expression" . L.lineFold scn $ \sc' -> topExpr sc'
--    where
--        topExpr :: Parser () -> Parser (Expr SS)
--        topExpr = foldr (\ep p -> ep p) (error "you will never arrive at the truth") (cycle exprParsers)

--exprParsers :: [ExprParser]
--exprParsers = []

--                child parser                      ws parser
type ExprParser = (Parser() -> Parser (Expr SS)) -> Parser () -> Parser (Expr SS)

-- | parses a case expression
-- case e of
--     p -> e
--     p -> e
--     ...
pCase :: ExprParser
pCase child sc' = dbg "case" $ do
    ((e, ms), ss) <- wrapSS . L.indentBlock scn $ do
        pKeyword "case"
        e <- child sc'
        pKeywordWith sc' "of"
        return $ L.IndentSome Nothing (return . (e,)) pMatch
    return (Case e ms ss)

-- | parses p -> e
pMatch :: Parser (Expr SS, Expr SS)
pMatch = do
    -- maybe you should take in two children or just have a pPatternExpr
    lhs <- pPatternExpr -- need fresh line fold here
    pReservedOp "->" -- TODO line fold? hope not
    rhs <- pExpr -- need fresh line fold here
    return (lhs, rhs)

-- | let decls in e
pLet :: ExprParser
pLet child sc' = dbg "let" $ do
    ((decls, body), ss) <- wrapSS $ do
        decls <- L.indentBlock scn $ do
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
    L.indentBlock scn $ do
        pKeywordWith sc' "where"
        return pDeclsHelp

-- | if e then e else e
pIf :: ExprParser
pIf child sc' = dbg "if" $ do
    ((cnd,thn,els), ss) <- wrapSS $ do
        pKeywordWith sc' "if"
        cnd <- child sc'
        pKeywordWith sc' "then"
        thn <- child sc'
        pKeywordWith sc' "else"
        els <- child sc'
        return (cnd,thn,els)
    return $ If cnd thn els ss

-- | e | e
pOr :: ExprParser
pOr child sc' = dbg "or" $ do
    (es, ss) <- wrapSS $ (child sc' <* vot sc') `sepBy1` pReservedOp "|" -- TODO <* necessary?
    return $ Or es ss

-- | e :: t
pAnnot :: ExprParser
pAnnot child sc' = dbg "annot" $ do
    ((e,t), ss) <- wrapSS $ do
        e <- child sc'
        pReservedOpWith sc' "::"
        t <- pTypeWith sc'
        return (e, t)
    return $ Annot e t ss

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
pString = dbg "string" (wrapSSWith (uncurry PString) $ lexeme (between (char '"') (char '"') (many L.charLiteral)))



pType = undefined

pTypeWith = undefined

pDecl = undefined

pDeclsHelp :: L.IndentOpt Parser [b] b
pDeclsHelp = L.IndentSome Nothing return pDecl
