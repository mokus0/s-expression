{-# LANGUAGE
        RankNTypes,
        FlexibleInstances,
        FlexibleContexts, 
        GeneralizedNewtypeDeriving
  #-}

module Codec.Sexpr.Token where

import Codec.Sexpr.Type
import Codec.Sexpr.Token.Class
import Codec.Sexpr.Parser
import Codec.Sexpr.Printer
import Control.Applicative
import qualified Codec.Binary.Base64.String as B64
import Data.List (intersperse)

import Control.Monad.Identity (Identity)
import Text.Parsec (runParser)

import Text.PrettyPrint

import Data.Binary
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

-------------------------------------------
-- Public convenience functions: Parsers --
-------------------------------------------

canonicalSexpr :: (AtomToken (Raw s)) => CharParser (SExpr [] s)
canonicalSexpr = parseSExprAs fromRaw id

advancedSexpr :: (AtomToken (Simple s)) => CharParser (SExpr [] s)
advancedSexpr = parseSExprAs fromSimple id

--  I prefer the derived instance, for consistency and for transparency,
--  but this is possible too:
--
-- instance (AtomToken (Simple a), 
--           ListToken l, Functor l) => 
--          Show (SExpr l a) where
--     showsPrec p = showSExprAs Simple id

-------------------------------------------
-- Public convenience functions: Parsing --
-------------------------------------------

readCanonicalSexprString :: (AtomToken (Raw s)) => String -> SExpr [] s
readCanonicalSexprString str = 
    case runParser canonicalSexpr () "<readCanonicalSexprString>" str of
        Left err -> error (show err)
        Right ok -> ok

readSexprString :: (AtomToken (Simple s)) => String -> SExpr [] s
readSexprString str = 
    case runParser advancedSexpr () "<readSexprString>" str of
        Left err -> error (show err)
        Right ok -> ok

readSexpr :: Read a => String -> SExpr [] a
readSexpr = fmap read . readSexprString

-- instance (AtomToken (Simple a), 
--           ListToken l, Functor l) => 
--          Read (SExpr l a) where
--     readsPrec p = readSExprAs fromSimple id

----------------------------------------------
-- Public convenience functions: Formatting --
----------------------------------------------

basicString :: (AtomToken (Raw s)) => SExpr [] s -> String
basicString s = render (basic s)

basic :: (AtomToken (Raw s)) => SExpr [] s -> Doc
basic s
    = braces (text (B64.encode . canonical s $ ""))


canonicalString :: (AtomToken (Raw s)) => SExpr [] s -> String
canonicalString sexpr = canonical sexpr ""

canonical :: (AtomToken (Raw s)) => SExpr [] s -> ShowS
canonical sexpr = showSExprAs Raw id sexpr

advancedString :: (AtomToken (Simple s)) => SExpr [] s -> String
advancedString s = render $ advanced s

advanced :: (AtomToken (Simple s)) => SExpr [] s -> Doc
advanced = printSExprAs Simple id

---------------------------------------------------
-- Public convenience functions: Binary Decoding --
---------------------------------------------------

---------------------------------------------------
-- Public convenience functions: Binary Encoding --
---------------------------------------------------

putCanonical :: (AtomToken (Raw s)) => SExpr [] s -> Put
putCanonical sexpr = putSExprAs Raw id sexpr

--------------------------
-- Atom implementations --
--------------------------

-- Hinted atoms
instance (AtomToken h, AtomToken s) => AtomToken (Hinted h s) where
    printAtom (UnHinted s) = printAtom s
    printAtom (Hinted h s) = brackets (printAtom h) <> printAtom s
    putAtom = putHinted
    parseAtom = hintedAtom

-- |Raw strings for the canonical and basic encodings
newtype Raw s = Raw { fromRaw :: s }
instance AtomToken (Raw String) where
    showsAtom (Raw s) = raw s
    printAtom (Raw s) = rawDoc s
    putAtom   (Raw s) = putRaw s
    parseAtom = Raw <$> rawString

instance AtomToken (Raw B.ByteString) where
    printAtom (Raw s) = rawDocBS s
    putAtom   (Raw s) = putRawBS s
    parseAtom = Raw . B.pack <$> rawString

instance AtomToken (Raw s) => AtomToken (Raw (Hinted s s)) where
    showsAtom (Raw hs) = showsAtom (mapHint Raw Raw hs)
    printAtom (Raw hs) = printAtom (mapHint Raw Raw hs)
    putAtom   (Raw hs) = putAtom   (mapHint Raw Raw hs)
    parseAtom = (Raw . mapHint fromRaw fromRaw) <$> parseAtom

-- | <simple-string>s in the "advanced" encoding.  No formatting information is
-- retained.
data Simple s = Simple { fromSimple :: s }
instance AtomToken (Simple String) where
    printAtom (Simple s) = format s
    parseAtom = Simple <$> simpleString
    
instance AtomToken (Simple s) => AtomToken (Simple (Hinted s s)) where
    showsAtom (Simple hs) = showsAtom (mapHint Simple Simple hs)
    printAtom (Simple hs) = printAtom (mapHint Simple Simple hs)
    parseAtom = (Simple . mapHint fromSimple fromSimple) <$> parseAtom


--------------------------
-- List implementations --
--------------------------

-- Plain lists
instance ListToken [] where
    showsList = showParen True . unwords'
        where
            unwords' [] = id
            unwords' [x] = x
            unwords' (x:xs) = x . showChar ' ' . unwords' xs
    printList = parens . sep
    putList l = do
        put '('
        sequence_ (intersperse (put ' ') l)
        put ')'
    parseList = sexprList

