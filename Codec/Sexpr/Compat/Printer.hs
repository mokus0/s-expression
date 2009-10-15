{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}

-- | Export S-expressions in any of the three ordinary forms: 
-- 
-- * Canonical, where a different string implies a different meaning
-- 
-- * Basic, suitable for transport over 7-bit and awkward media
-- 
-- * Advanced, a human-readable pretty-printed encoding.
-- 
-- The @-> 'String'@ functions are probably what you want unless you
-- know you want something else.
-- 
-- The 'Show' instance for Sexpr is provided here, using
-- advancedString and the underlying Show instance.  Overlapping
-- instances are used to provide a nice show for @'Sexpr' 'String'@.

module Codec.Sexpr.Compat.Printer where

import Codec.Sexpr.Compat.Internal

import Data.Binary.Put
import qualified Data.ByteString.Char8 as B

import Data.Char
import Data.Maybe
import Text.PrettyPrint
import qualified Codec.Binary.Base64.String as B64

instance Show (Sexpr String) where
    show s = advancedString s

instance Show s => Show (Sexpr s) where
    show s = advancedString $ fmap show s

raw :: String -> ShowS
raw s = shows (length s) . showString ":" . showString s

canonicalString :: Sexpr String -> String
canonicalString s = canonical s ""

canonical :: Sexpr String -> ShowS
canonical s | isAtom s && hint s == Just defaultHint = raw $ unAtom s
canonical s | isAtom s = showString "[" 
                         . raw (fromJust $ hint s)
                         . showString "]"
                         . raw (unAtom s)
canonical s | otherwise = showString "("
                 . showString (foldr (.) id (map canonical $ unList s) $ "")
                 . showString ")"

putRaw :: String -> Put
putRaw s = do
  putByteString . B.pack . show $ length s
  putChar' ':'
  putByteString (B.pack s)

putRawBS :: B.ByteString -> Put
putRawBS s = do
  putByteString . B.pack . show $ B.length s
  putChar' ':'
  putByteString s

putChar' :: Char -> Put
putChar' = putWord8 . fromIntegral . ord

putCanonical :: Sexpr String -> Put
putCanonical = putCanonicalHelper putRaw

putCanonicalBS :: Sexpr B.ByteString -> Put
putCanonicalBS = putCanonicalHelper putRawBS

putCanonicalHelper :: (a -> Put) -> Sexpr a -> Put
putCanonicalHelper putRaw' s | isAtom s && hint s == 
                               Just defaultHint = putRaw' $ unAtom s
putCanonicalHelper putRaw' s | isAtom s = do
  putChar' '['
  putRaw (fromJust $ hint s)
  putChar' ']'
  putRaw' (unAtom s)
putCanonicalHelper putRaw' s | otherwise = do
  putChar' '('
  mapM_ (putCanonicalHelper putRaw') $ unList s
  putChar' ')'

basicString :: Sexpr String -> String
basicString s = render $ basic s

basic :: Sexpr String -> Doc
basic s = braces . hcat $ map char . B64.encode $ canonical s ""
-- FIXME should basic add and encode a NUL terminator?
-- FIXME We parse it out in canonical---should canonical encodings end with a NUL? 

advancedString :: Sexpr String -> String
advancedString s = render $ advanced s

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
             || fromIntegral (length (show s)) * 10 <= (length s) * 11

canHex :: String -> Bool
canHex s = length s `elem` [1,2,3,4,8,16,20]

hex :: String -> Doc
hex s = text (show $ length s) <> (char '#') <> hcat (map (text . hexEncode) s) <> (char '#')

hexEncode :: Char -> String
hexEncode x = (intToDigit h) : (intToDigit o) : []
    where 
      (h,o) = quotRem (ord x) 16

quote :: Show a => a -> Doc
quote s = text $ show s

base64 :: String -> Doc
base64 s = (char '|') <> hcat (map char $ B64.encode s) <> (char '|')

advanced :: Sexpr String -> Doc
advanced s | isAtom s && hint s == Just defaultHint = format $ unAtom s
advanced s | isAtom s = brackets (format $ fromJust $ hint s) 
                        <> (format $ unAtom s) 
advanced s | otherwise = parens $ sep (map advanced $ unList s)
