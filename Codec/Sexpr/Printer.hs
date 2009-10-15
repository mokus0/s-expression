{-# LANGUAGE RankNTypes #-}
module Codec.Sexpr.Printer where

import Codec.Sexpr.Type
import Codec.Sexpr.Token.Class
import Text.PrettyPrint
import qualified Codec.Binary.Base64.String as B64
import Data.Char (ord, intToDigit)
import Numeric (showOct)

import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString.Char8 as B

raw :: String -> ShowS
raw s = shows (length s) . showString ":" . showString s

rawDoc :: String -> Doc
rawDoc s = int (length s) <> char ':' <> text s

rawDocBS :: B.ByteString -> Doc
rawDocBS s = int (B.length s) <> char ':' <> text (B.unpack s)

format :: String -> Doc
format s | canToken s = text s
         | canQuote s = quote s
         | canHex s = hex s
         | otherwise = base64 s

canToken :: String -> Bool
canToken (x:xs) = isInitialTokenChar x && all isTokenChar xs
canToken [] = False

canQuote :: String -> Bool
canQuote s = all isQuoteableChar s
             || (length (show s)) * 10 <= (length s) * 11

canHex :: String -> Bool
canHex s = length s `elem` [1,2,3,4,8,16,20]

hex :: String -> Doc
hex s = text (show $ length s) <> (char '#') <> hcat (map (text . hexEncode) s) <> (char '#')

hexEncode :: Char -> String
hexEncode x = (intToDigit h) : (intToDigit o) : []
    where 
      (h,o) = quotRem (ord x) 16

quote :: String -> Doc
quote s = text $ showQuotedString s ""

-- 'show' uses decimal escapes, as well as a lot of
-- other special haskelly escapes that rivest's s-expression
-- grammar does not include.
-- TODO: This almost certainly does not properly handle unicode,
-- assuming that "properly" in this context is even well-defined.
showQuotedString :: String -> ShowS
showQuotedString x
    = showChar '\"' 
    . foldr (\c cs -> showQuotedChar c . cs) id x
    . showChar '\"' 

showQuotedChar                :: Char -> ShowS
showQuotedChar c | c >= '\DEL'  =  octEsc c
showQuotedChar '\\'             =  showString "\\\\"
showQuotedChar '\''             =  showString "\\'"
showQuotedChar '\"'             =  showString "\\\""
showQuotedChar c | c >= ' '     =  showChar c
showQuotedChar '\b'             =  showString "\\b"
showQuotedChar '\t'             =  showString "\\t"
showQuotedChar '\v'             =  showString "\\v"
showQuotedChar '\n'             =  showString "\\n"
showQuotedChar '\f'             =  showString "\\f"
showQuotedChar '\r'             =  showString "\\r"
showQuotedChar c                =  octEsc c

octEsc :: Char -> ShowS
octEsc c = showChar '\\' . show3Oct (ord c)
    where show3Oct n
            | n > 255 {- 377 oct -}
            = error "octEsc called for multi-byte char"
            | n > 63  {- 77 oct -}
            = showOct n
            | n > 7   {- 7 oct -}
            = showChar '0' . showOct n
            | otherwise
            = showString "00" . showOct n
    

base64 :: String -> Doc
base64 s = (char '|') <> hcat (map char $ B64.encode s) <> (char '|')

---------------------------------------------------
-- Public convenience functions: Binary Decoding --
---------------------------------------------------

---------------------------------------------------
-- Public convenience functions: Binary Encoding --
---------------------------------------------------

putRaw :: String -> Put
putRaw s = do
  putByteString . B.pack . show $ length s
  put ':'
  putByteString (B.pack s)

putRawBS :: B.ByteString -> Put
putRawBS s = do
  putByteString . B.pack . show $ B.length s
  put ':'
  putByteString s

putHinted :: (AtomToken h, AtomToken a) => Hinted h a -> Put
putHinted (UnHinted s) = putAtom s
putHinted (Hinted h s) = do
    put '['
    putAtom h
    put ']'
    putAtom s
