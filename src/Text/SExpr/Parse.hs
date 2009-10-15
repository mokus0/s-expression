{-# LANGUAGE CPP, RankNTypes, FlexibleContexts #-}

-- |This module defines the parts of the S-expression grammar
-- necessary to construct a complete parser, but does not actually construct
-- a complete parser.  Useful parsers with a fair amount of useful polymorphism
-- are provided by the @Token@ module.
module Text.SExpr.Parse where

import Text.SExpr.Type
import Text.SExpr.Convert.Classes
import Data.Char
import Control.Applicative hiding ((<|>), many, optional)
import Control.Monad
import Numeric (readOct, readDec, readHex)
import qualified Codec.Binary.Base64.String as B64
import Data.Maybe (catMaybes)

import Text.Parsec

-- Based on syntax given in http://people.csail.mit.edu/rivest/Sexp.txt

-- For canonical and basic transport:
-- <sexpr>    	:: <string> | <list>
-- <simple-string>	:: <raw> ;
-- <string>   	:: <display>? <simple-string> ;

sexpr :: (Atom a, List l) => CharParser st (SExpr l a)
sexpr = (Atom <$> parseAtom) 
    <|> (List <$> parseList sexpr)
    <|> basicTransport sexpr

basicTransport :: CharParser () x -> CharParser st x
basicTransport sexp = between (char '{' >> whitespace) (char '}') $ do
    b64 <- many (padded base64Char)
    let str = B64.decode b64
        subSrcName = "subSrcName"
        subParser = do
            whitespace
            result <- sexp
            whitespace
            eof
            return result
    case runParser subParser () subSrcName str of
        -- Error reporting leaves much to be desired, but I haven't found a way to improve it yet.
        Left err -> fail ("error in basic transport: " ++ show err)
        Right result -> return result

-- For advanced transport:

-- <sexpr>    	:: <string> | <list>
-- <simple-string>	:: <raw> | <token> | <base-64> | <hexadecimal> | 
-- 		           <quoted-string> ;
simpleString :: CharParser st String
simpleString =  try rawString
            <|> try base64String
            <|> try quotedString
            <|> try tokenString
            <|> hexadecimalString

-- Common:

hintedAtom :: (Atom hint, Atom atom) => CharParser st (Hinted hint atom)
hintedAtom  = do
        mbHint <- optionMaybe display
        x <- parseAtom
        case mbHint of
            Nothing -> return (Unhinted x)
            Just h  -> return (Hinted h x)
    <?> "hinted atom"

-- <display>  	:: "[" <simple-string> "]" ;
display :: Atom atom => CharParser st atom
display = between (char '[') (char ']') parseAtom
       <?> "display hint"

-- <raw>      	:: <decimal> ":" <bytes> ;
-- <bytes> 	-- any string of bytes, of the indicated length
rawString :: CharParser st String
rawString = do
        n <- decimal
        char ':'
        count n anyChar
    <?> "raw string"

-- <decimal>  	:: <decimal-digit>+ ;
decimal :: Integral a => CharParser st a
decimal = readIt <$> many1 digit <?> "decimal"
    where readIt s = case readDec s of
            [(n,"")] -> n
            _ -> error "programming error: 'decimal' parser matched a non-decimal string"

-- 		-- decimal numbers should have no unnecessary leading zeros
-- <token>    	:: <tokenchar>+ ;
-- <tokenchar>  	:: <alpha> | <decimal-digit> | <simple-punc> ;
-- <alpha>       	:: <upper-case> | <lower-case> | <digit> ;
-- <lower-case>  	:: "a" | ... | "z" ;
-- <upper-case>  	:: "A" | ... | "Z" ;
-- <decimal-digit> :: "0" | ... | "9" ;
-- <simple-punc> 	:: "-" | "." | "/" | "_" | ":" | "*" | "+" | "=" ;
-- (the grammar doesn't say, but elsewhere in the text it indicates that
-- tokens must not start with digits)
tokenString :: CharParser st String
tokenString = liftM2 (:) initialTokenChar (many tokenChar) <?> "token string"
initialTokenChar :: CharParser st Char
initialTokenChar = satisfy isInitialTokenChar <?> "initial token character"
tokenChar :: CharParser st Char
tokenChar = satisfy isTokenChar <?> "token character"

-- <base-64>  	:: <decimal>? "|" ( <base-64-char> | <whitespace> )* "|" ;
-- <base-64-char> 	:: <alpha> | <decimal-digit> | "+" | "/" | "=" ;
base64String :: CharParser st String
base64String = do
    n <- optionMaybe decimal
    char '|'
    whitespace
    encoded <- many (padded base64Char) <?> "base-64 encoded string"
    char '|'
    let decoded = B64.decode encoded
    case n of
        Nothing     -> return decoded
        Just len    -> do
            when (len /= length decoded) $ fail "base64-encoded string length annotation does not match length of encoded string"
            return decoded
base64Char :: CharParser st Char
base64Char = alphaNum <|> oneOf "+/=" 
    <?> "base-64 character"

-- <hexadecimal>   :: "#" ( <hex-digit> | <white-space> )* "#" ;
-- <hex-digit>     :: <decimal-digit> | "A" | ... | "F" | "a" | ... | "f" ;
hexadecimalString :: CharParser st String
hexadecimalString = do
    mbLen <- optionMaybe decimal
    hexes <- between (char '#' >> whitespace) (char '#') (many (padded hexDigit))
    when (odd (length hexes)) $ fail "hex string has odd length"
    
    let pair [] = []
        pair [_] = error "programming error: in hexadecimal parser, an odd-length hex string was not rejected"
        pair (x:y:rest) = [x,y] : pair rest
        
        readIt str = case readHex str of
            [(x, "")] -> x
            _ -> error "programming error: hexadecimal parser matched non-hex content"
        
        decoded = map (chr . readIt) (pair hexes)
    
    case mbLen of
        Nothing -> return decoded
        Just len
            | len /= length decoded
            -> fail "hexadecimal-encoded string length annotation does not match length of encoded string"
            | otherwise
            -> return decoded
    

-- <quoted-string> :: <decimal>? <quoted-string-body>  
-- <quoted-string-body> :: "\"" <bytes> "\""
quotedString :: CharParser st String
quotedString = (optionMaybe decimal >>= quotedStringBody) <?> "quoted string"

quotedStringBody :: Maybe Int -> CharParser st String
quotedStringBody len = between (char '"' <?> "start of quoted string") (char '"' <?> "end of string") (quotedStringContent len)

-- Spec is not clear; should unescaped newlines be accepted or rejected?
quotedStringContent :: Maybe Int -> CharParser st String
quotedStringContent Nothing = catMaybes <$> many (quotedChar <|> Just <$> unquotedChar)
quotedStringContent (Just len) = go len
    where
        go 0 = do
            mbChar <- optionMaybe (quotedChar <?> "escaped newline")
            case mbChar of
                Nothing -> return []
                Just Nothing -> go 0
                Just (Just x)  -> unexpected [x]
        go n = do
            mbChar <- quotedChar <|> Just <$> unquotedChar
            case mbChar of
                Nothing -> go n
                Just ch -> liftM (ch:) (go (n-1))

quotedChar :: CharParser st (Maybe Char)
quotedChar = (char '\\' >> anyChar >>= escaped) <?> "escaped character"
    where
        escaped 'b'  = return $ Just '\b'
        escaped 't'  = return $ Just '\t'
        escaped 'v'  = return $ Just '\v'
        escaped 'n'  = return $ Just '\n'
        escaped 'f'  = return $ Just '\f'
        escaped 'r'  = return $ Just '\r'
        escaped '\'' = return $ Just '\''
        escaped '\"' = return $ Just '\"'
        escaped '\\' = return $ Just '\\'
        escaped x | x `elem` "0123" = do
            xs <- count 2 octDigit
            case readOct (x:xs) of
                [(y,"")] -> return (Just $ chr y)
                _ -> error "programming error: quotedChar parser tried to read an invalid octal escape string"
        escaped 'x' = do
            xs <- count 2 hexDigit
            case readHex xs of
                [(x,"")] -> return (Just $ chr x)
                _ -> error "programming error: quotedChar parser tried to read an invalid hexadecimal escape string"
        escaped '\r' = do
            optional (char '\n')
            return Nothing
        escaped '\n' = do
            optional (char '\r')
            return Nothing
        escaped other = fail ("unrecognized escape sequence \"\\" ++ other : "\"")
            
unquotedChar :: CharParser st Char
unquotedChar = noneOf "\"\\"

-- <list>     	:: "(" ( <sexp> | <whitespace> )* ")" ;
sexprList :: CharParser st a -> CharParser st [a]
sexprList sexp = between (char '(' >> whitespace) (char ')')
        (many (padded sexp))
    <?> "list"

-- <whitespace> 	:: <whitespace-char>* ;
-- <whitespace-char> :: " " | "\t" | "\r" | "\n" ;
whitespace :: CharParser st String
whitespace = many (oneOf " \t\r\n") <?> "whitespace"

padded :: CharParser st a -> CharParser st a
padded x = x >>= \a -> whitespace >> return a
