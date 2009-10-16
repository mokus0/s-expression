{-# LANGUAGE RankNTypes #-}
module Text.SExpr.Print where

import Text.SExpr.Type
import Text.SExpr.Convert.Classes
import Text.PrettyPrint
import qualified Codec.Binary.Base64.String as B64
import Data.Char (ord, intToDigit)
import Numeric (showOct)
import Data.List (intersperse)

import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

-----------------------------------
-- Utility functions: Formatting --
-----------------------------------

-- |Format a hinted atom using the Atom instances for the hint and the atom.
showsHinted :: (Atom h, Atom s) => Hinted h s -> ShowS
showsHinted (Unhinted s) = showsAtom s
showsHinted (Hinted h s) = showChar '[' . showsAtom h . showChar ']' . showsAtom s

-- |Format a list with spaces for the advanced encoding
showsSimpleList :: [ShowS] -> ShowS
showsSimpleList = showParen True . foldl (.) id . intersperse (showChar ' ')

-- |Format a list without spaces for the canonical encoding
showsCanonicalList :: [ShowS] -> ShowS
showsCanonicalList = showParen True . foldl (.) id

-- |Format a hinted atom using the Atom instances for the hint and the atom.
printHinted :: (Atom h, Atom s) => Hinted h s -> Doc
printHinted (Unhinted s) = printAtom s
printHinted (Hinted h s) = brackets (printAtom h) <> printAtom s

-- |Format a list with spaces for the advanced encoding
printSimpleList :: [Doc] -> Doc
printSimpleList = parens . sep

-- |Format a list without spaces for the canonical encoding
printCanonicalList :: [Doc] -> Doc
printCanonicalList = parens . cat

-- |Format a string using the "raw" encoding
raw :: String -> ShowS
raw s = shows (length s) . showChar ':' . showString s

-- |Format a strict bytestring using the "raw" encoding
rawBS :: B.ByteString -> ShowS
rawBS s = shows (B.length s) . showChar ':' . showString (B.unpack s)

-- |Format a lazy bytestring using the "raw" encoding
rawBSL :: BL.ByteString -> ShowS
rawBSL s = shows (BL.length s) . showChar ':' . showString (BL.unpack s)

-- |Format a string using the "raw" encoding
rawDoc :: String -> Doc
rawDoc s = int (length s) <> colon <> text s

-- |Format a strict bytestring using the "raw" encoding
rawDocBS :: B.ByteString -> Doc
rawDocBS s = int (B.length s) <> colon <> text (B.unpack s)

-- |Format a lazy bytestring using the "raw" encoding
rawDocBSL :: BL.ByteString -> Doc
rawDocBSL s = integer (toInteger $ BL.length s) <> colon <> text (BL.unpack s)

-- |Format a string using the "advanced" encoding
format :: String -> Doc
format s | canToken s = text s
         | canQuote s = quote s
         | canHex s = hex s
         | otherwise = base64 s

-- |Determine whether a string atom can be encoded as a bare token
canToken :: String -> Bool
canToken (x:xs) = isInitialTokenChar x && all isTokenChar xs
canToken [] = False

-- |Determine whether a string atom can/should be encoded as a quoted string
canQuote :: String -> Bool
canQuote s = all isQuoteableChar s
             || (length (show s)) * 10 <= (length s) * 11

-- |Determine whether a string atom can/should be encoded as a hexadecimal string
canHex :: String -> Bool
canHex s = length s `elem` [1,2,3,4,8,16,20]

-- |Encode a string atom as a hexadecimal string
hex :: String -> Doc
hex s = int (length s) <> (char '#') <> text (hexEncodeString s "") <> (char '#')

-- |Encode a 'String' as a hexadecimal string
hexEncodeString :: String -> ShowS
hexEncodeString = foldr (\c cs -> hexEncodeChar c . cs) id

-- |Encode a 'Char' as a hexadecimal string
hexEncodeChar :: Char -> ShowS
hexEncodeChar x = showChar (intToDigit h) . showChar (intToDigit o)
    where 
      (h,o) = quotRem (ord x) 16

-- |Encode a string atom as a quoted string
quote :: String -> Doc
quote s = text $ showQuotedString s ""

-- |'show' uses decimal escapes, as well as a lot of
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

-- |Escape a single character as a backslash and a 3-digit octal string
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
    

-- |Encode a string atom using the base-64 format
base64 :: String -> Doc
base64 s = (char '|') <> hcat (map char $ B64.encode s) <> (char '|')

----------------------------------------
-- Utility functions: Binary Decoding --
----------------------------------------

----------------------------------------
-- Utility functions: Binary Encoding --
----------------------------------------

-- |Format a string using the "raw" encoding
putRaw :: String -> Put
putRaw s = do
  putByteString . B.pack . show $ length s
  put ':'
  putByteString (B.pack s)

-- |Format a strict bytestring using the "raw" encoding
putRawBS :: B.ByteString -> Put
putRawBS s = do
  putByteString . B.pack . show $ B.length s
  put ':'
  putByteString s

-- |Format a lazy bytestring using the "raw" encoding
putRawBSL :: BL.ByteString -> Put
putRawBSL s = do
  putLazyByteString . BL.pack . show $ BL.length s
  put ':'
  putLazyByteString s

-- |Encode a hinted atom to a binary stream, using the Atom instances
-- for the hint and the atom
putHinted :: (Atom h, Atom a) => Hinted h a -> Put
putHinted (Unhinted s) = putAtom s
putHinted (Hinted h s) = do
    put '['
    putAtom h
    put ']'
    putAtom s

putSimpleList xs = do
    put '('
    sequence_ (intersperse (put ' ') xs)
    put ')'

putCanonicalList :: [Put] -> Put
putCanonicalList xs = put '(' >> sequence_ xs >> put ')'