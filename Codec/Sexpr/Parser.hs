{-# LANGUAGE RankNTypes, FlexibleContexts #-}

-- |This module defines the parts of the S-expression grammar
-- necessary to construct a complete parser, but does not actually construct
-- a complete parser.  Useful parsers with a fair amount of useful polymorphism
-- are provided by the @Token@ module.
module Codec.Sexpr.Parser where

import Codec.Sexpr.Type
import Codec.Sexpr.Token.Class
import Text.Parsec
import Data.Char
import Control.Applicative hiding ((<|>), many, optional)
import Control.Monad
import Numeric (readOct, readDec, readHex)
import qualified Codec.Binary.Base64.String as B64
import Data.Maybe (catMaybes)

--------------------
-- Parsec parsers --
--------------------

-- Based on syntax given in http://people.csail.mit.edu/rivest/Sexp.txt

-- For canonical and basic transport:
-- <sexpr>    	:: <string> | <list>
-- <simple-string>	:: <raw> ;
-- <string>   	:: <display>? <simple-string> ;

-- For advanced transport:

-- <sexpr>    	:: <string> | <list>
-- <simple-string>	:: <raw> | <token> | <base-64> | <hexadecimal> | 
-- 		           <quoted-string> ;
simpleString :: CharParser String
simpleString =  try rawString
            <|> try base64String
            <|> try quotedString
            <|> try tokenString
            <|> hexadecimalString

-- Common:

hintedAtom :: (AtomToken hint, AtomToken atom) => CharParser (Hinted hint atom)
hintedAtom  = do
        mbHint <- optionMaybe display
        x <- parseAtom
        case mbHint of
            Nothing -> return (UnHinted x)
            Just h  -> return (Hinted h x)
    <?> "hinted atom"

-- <display>  	:: "[" <simple-string> "]" ;
display :: AtomToken atom => CharParser atom
display = between (char '[') (char ']') parseAtom
       <?> "display hint"

-- <raw>      	:: <decimal> ":" <bytes> ;
-- <bytes> 	-- any string of bytes, of the indicated length
rawString :: CharParser String
rawString = do
        n <- decimal
        char ':'
        count n anyChar
    <?> "raw string"

-- <decimal>  	:: <decimal-digit>+ ;
decimal :: Integral a => CharParser a
decimal = read <$> many1 digit <?> "decimal"
    where read s = case readDec s of
            [(n,"")] -> n

-- 		-- decimal numbers should have no unnecessary leading zeros
-- <token>    	:: <tokenchar>+ ;
-- <tokenchar>  	:: <alpha> | <decimal-digit> | <simple-punc> ;
-- <alpha>       	:: <upper-case> | <lower-case> | <digit> ;
-- <lower-case>  	:: "a" | ... | "z" ;
-- <upper-case>  	:: "A" | ... | "Z" ;
-- <decimal-digit> :: "0" | ... | "9" ;
-- <simple-punc> 	:: "-" | "." | "/" | "_" | ":" | "*" | "+" | "=" ;
tokenString :: CharParser String
tokenString = liftM2 (:) initialTokenChar (many tokenChar)
initialTokenChar :: CharParser Char
initialTokenChar = satisfy isInitialTokenChar
tokenChar :: CharParser Char
tokenChar = satisfy isTokenChar

-- <base-64>  	:: <decimal>? "|" ( <base-64-char> | <whitespace> )* "|" ;
-- <base-64-char> 	:: <alpha> | <decimal-digit> | "+" | "/" | "=" ;
base64String :: CharParser String
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
base64Char :: CharParser Char
base64Char = alphaNum <|> oneOf "+/="            

-- <hexadecimal>   :: "#" ( <hex-digit> | <white-space> )* "#" ;
-- <hex-digit>     :: <decimal-digit> | "A" | ... | "F" | "a" | ... | "f" ;
hexadecimalString :: CharParser String
hexadecimalString = do
    mbLen <- optionMaybe decimal
    hexes <- between (char '#' >> whitespace) (char '#') (many (padded hexDigit))
    when (odd (length hexes)) $ fail "hex string has odd length"
    
    let pair [] = []
        pair (x:y:rest) = [x,y] : pair rest
        readHex' str = case readHex str of [(x, "")] -> x
        decoded = map (chr . readHex') (pair hexes)
    
    case mbLen of
        Nothing -> return decoded
        Just len
            | len /= length decoded
            -> fail "hexadecimal-encoded string length annotation does not match length of encoded string"
            | otherwise
            -> return decoded
    

-- <quoted-string> :: <decimal>? <quoted-string-body>  
-- <quoted-string-body> :: "\"" <bytes> "\""
quotedString :: CharParser String
quotedString = optionMaybe decimal >>= quotedStringBody

quotedStringBody :: Maybe Int -> CharParser String
quotedStringBody len = between (char '"' <?> "start of quoted string") (char '"' <?> "end of string") (quotedStringContent len)

-- Spec is not clear; should unescaped newlines be accepted or rejected?
quotedStringContent :: Maybe Int -> CharParser String
quotedStringContent Nothing = catMaybes <$> many (quotedChar <|> Just <$> unquotedChar)
quotedStringContent (Just len) = go len
    where
        go 0 = do
            mbChar <- optionMaybe (quotedChar <?> "escaped newline")
            case mbChar of
                Nothing -> return []
                Just Nothing -> go 0
                Just (Just x)  -> fail ("unexpected " ++ show [x])
        go n = do
            mbChar <- quotedChar <|> Just <$> unquotedChar
            case mbChar of
                Nothing -> go n
                Just ch -> liftM (ch:) (go (n-1))

quotedChar :: CharParser (Maybe Char)
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
        escaped 'x' = do
            xs <- count 2 hexDigit
            case readHex xs of
                [(x,"")] -> return (Just $ chr x)
        escaped '\r' = do
            optional (char '\n')
            return Nothing
        escaped '\n' = do
            optional (char '\r')
            return Nothing
        escaped other = fail ("unrecognized escape sequence \"\\" ++ other : "\"")
            
            
unquotedChar :: CharParser Char
unquotedChar = noneOf "\"\\"

-- <list>     	:: "(" ( <sexp> | <whitespace> )* ")" ;
sexprList :: CharParser a -> CharParser [a]
sexprList sexp = between (char '(' >> whitespace) (char ')')
        (many (padded sexp))
    <?> "list"

-- <whitespace> 	:: <whitespace-char>* ;
-- <whitespace-char> :: " " | "\t" | "\r" | "\n" ;
whitespace :: CharParser String
whitespace = many (oneOf " \t\r\n") <?> "whitespace"

padded :: CharParser a -> CharParser a
padded x = x >>= \a -> whitespace >> return a

