module Codec.Sexpr.Compat.Parser where

import Codec.Sexpr.Compat.Internal
import Control.Monad

-- import Data.Binary.Get
-- import Data.ByteString

import Data.Char
import Text.ParserCombinators.ReadP
import qualified Codec.Binary.Base64.String as B64


instance Read s => Read (Sexpr s) where
    readsPrec _ s = map (\(a,b) -> (fmap read a, b)) s'
        where 
          s' = readP_to_S advancedSexpr s :: [(Sexpr String,String)]

-- |Read a @'Sexpr' 'String'@ in any encoding: Canonical, Basic, or Advanced.
readSexprString :: String -> Sexpr String
readSexprString s = case readP_to_S advancedSexpr s of
                      [] -> error $ "Cannot parse sexpr from: " ++ s ++ "."
                      s' -> fst $ head s'

-- |Read a @'Sexpr' 'String'@ in canonical encoding.
readCanonicalSexprString :: String -> Sexpr String
readCanonicalSexprString s = case readP_to_S canonicalSexpr s of
                      [] -> error 
                            $ "Cannot parse canonical sexpr from: " ++ s ++ "."
                      s' -> fst $ head s'


-- |Read a @'Sexpr' a@ using the 'Read' instance for @a@.  The Sexpr
-- may be in any encoding: Canonical, Basic, or Advanced.
readSexpr :: Read a => String -> Sexpr a
readSexpr = fmap read . readSexprString

-- |Parser for @'Sexpr' 'String'@s suitable for embedding in other 
-- @ReadP@ parsers.
sexpr :: Bool -> ReadP (Sexpr String)
sexpr b = do
  s <- (internalSexpr b)
  when b skipSpaces
  return s

{-
getCanonicalAtom :: Get (Sexpr ByteString)
getCanonicalAtom = do
  l <- getDecimal
  skip 1 -- ':'
  s <- getLazyByteString l -- FIXME doesn't handle hints
  return $ atom s

getCanonicalList :: Get S
getCanonicalList = do
  skip 1 -- '('
  -- FIXME mostly missing
-}

-- |For some applications it is wise to accept only very carefully
-- specified input.  This is useful when you know you are receiving
-- exactly a Canonical S-Expression.  It will read only a Canonical
-- S-expression (and optional terminating NUL), but not the Basic or
-- Advanced encodings.
canonicalSexpr :: ReadP (Sexpr String)
canonicalSexpr = sexpr False

advancedSexpr :: ReadP (Sexpr String)
advancedSexpr = sexpr True

internalSexpr :: Bool -> ReadP (Sexpr String)
internalSexpr b = do
  s <- atomR b <++ listR b <++ basicTransport b
  optional $ char '\NUL'
  return s

basicTransport :: Bool -> ReadP (Sexpr String)
basicTransport b = do
  when b skipSpaces
  b64Octets <- between (char '{') (char '}') $ many b64char
  let parses = readP_to_S (sexpr b) $ B64.decode b64Octets
  choice $ map (return.fst) $ filter ((=="") . snd) parses

b64char :: ReadP Char
b64char = satisfy (\x -> isAlphaNum x || x `elem` "+/=")

b64char' :: ReadP Char
b64char' = skipSpaces >> b64char

hexchar :: ReadP Char
hexchar = satisfy isHexDigit

hexchar' :: ReadP Char
hexchar' = skipSpaces >> hexchar

listR :: Bool -> ReadP (Sexpr String)
listR b = do
  when b skipSpaces
  l <- between (char '(') ((when b skipSpaces) >> char ')') $ many (sexpr b)
  return $ list l

atomR :: Bool -> ReadP (Sexpr String)
atomR b = unhinted +++ hinted
  where 
    unhinted = simpleString b >>= (return . atom)
    hinted = do
      when b skipSpaces
      h <- between (char '[' >> skipSpaces) 
                   (skipSpaces >> char ']') 
                   (simpleString b)
      v <- simpleString b
      return $ hintedAtom h v
      
simpleString :: Bool -> ReadP String
simpleString False = raw
simpleString True =
  skipSpaces >> (raw +++ token +++ b64Atom +++ hexAtom +++ quotedString)

quotedString :: ReadP String
quotedString = withLength +++ withoutLength
  where
    withLength = do
      l <- decimal
      c <- between (char '"') (char '"') (many get)
      readString c (\s -> if (l == length s)
                             then return s
                             else fail "length error")
    withoutLength = do
                 c <- between (char '"') (char '"') (many get)
                 readString c (\s -> return s)
    readString c f = case filter (\x -> "" == snd x) $ reads ('"':c ++ "\"") of
        (s,""):_ -> f s
        _ -> pfail


hexAtom :: ReadP String
hexAtom = do
  s <- withLength +++ withoutLength
  return $ hexDecode s
    where
      withLength = do
            l <- decimal
            between (char '#') (char '#') (count (2*l) hexchar')
      withoutLength = between (char '#') (char '#') (many1 hexchar')

hexDecode :: String -> String
hexDecode [] = ""
hexDecode [o] = [chr $ digitToInt o]
hexDecode (h:o:cs) = chr (16*digitToInt h + digitToInt o) : (hexDecode cs)

b64Atom :: ReadP String
b64Atom = do
  s <- withLength +++ withoutLength
  return $ B64.decode s
  where
    withLength = do
      l <- decimal
      between (char '|') (char '|') (count (b64length l) b64char')
    withoutLength = 
      between (char '|') (char '|') (many b64char')
    b64length l = -4 * (l `div` (-3))
                  -- === 4 * (ceiling (fromIntegral l / 3)), without Double

token :: ReadP String
token = do
  c <- satisfy isInitialTokenChar
  cs <- munch isTokenChar
  return (c:cs)

raw :: ReadP String
raw = do
  len <- decimal
  char ':'
  count len get

decimal :: ReadP Int
decimal = do
  s <- munch1 isNumber
  return $ read s
