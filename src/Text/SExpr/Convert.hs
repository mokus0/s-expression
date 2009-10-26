{-# LANGUAGE
        RankNTypes,
        FlexibleInstances,
        FlexibleContexts, 
        GeneralizedNewtypeDeriving
  #-}

-- |The "Text.SExpr.Convert" module defines a modular parsing and
-- serializing framework based on two type classes, 'Atom' and 'List'.
--
-- Each type class defines a means of encoding and decoding the 
-- corresponding part of the S-Expression grammar.
-- 
-- Direct encoding support is not provided by default for String, et al,
-- but rather via type wrappers 'Raw' and 'Simple'.  By 'fmap'ing the
-- appropriate data constructor onto the 'SExpr', the mode of serialization
-- is selected.  Both modes are available for Strings and lazy and strict
-- ByteStrings, with and without display hints.  Polymorphic convenience 
-- functions such as 'canonicalSExpr' and 'advancedSExpr' are provided
-- which hide the details, allowing them to be used with most any supported
-- types.
--
-- Note that the choice of type influences the language used when parsing.
-- For example, when the destination type does not include hints, the parser
-- will not accept them.  Furthermore, new instances may introduce language 
-- extensions, either at the atom or list level.
module Text.SExpr.Convert
    ( module Text.SExpr.Convert
    , module Text.SExpr.Convert.Classes
    , module Text.SExpr.Convert.Instances
    ) where

import Text.SExpr.Type
import Text.SExpr.Parse
import Text.SExpr.Convert.Classes
import Text.SExpr.Convert.Instances
import qualified Codec.Binary.Base64.String as B64

import Control.Applicative ((<$>))

import Text.Parsec (runParser, char, eof, optional, many, anyChar)

import Data.Binary (Put)
import Text.PrettyPrint (Doc, render, braces, text)

-------------------------------------------
-- Public convenience functions: Parsers --
-------------------------------------------

parseSExpr :: (Atom a, List l) => CharParser () (SExpr l a)
parseSExpr = do
    s <- sexpr parseAtom parseList
    eof
    return s

parseSExprAs
  :: (Functor l1, Atom a1, List l1) =>
     (a1 -> a2)
     -> (l1 (SExpr l2 a2) -> l2 (SExpr l2 a2))
     -> CharParser () (SExpr l2 a2)
parseSExprAs a l = do
    s <- sexpr (a <$> parseAtom) (\s -> l <$> parseList s)
    eof
    return s

readsSExpr :: (Atom a, List l) => ReadS (SExpr l a)
readsSExpr str = case runParser parse () "<readSExpr>" str of
    Left _err   -> []
    Right x     -> [x]
    where 
        parse = do
            s <- sexpr parseAtom parseList
            optional (char '\NUL')
            rest <- many anyChar
            return (s, rest)

readsSExprAs
  :: (Functor l1, Atom a1, List l1) =>
     (a1 -> a2)
     -> (l1 (SExpr l2 a2) -> l2 (SExpr l2 a2))
     -> ReadS (SExpr l2 a2)
readsSExprAs a l str = case runParser parse () "<readSExprAs>" str of
    Left _err   -> []
    Right x     -> [x]
    where 
        parse = do
            s <- sexpr (a <$> parseAtom) (\s -> l <$> parseList s)
            optional (char '\NUL')
            rest <- many anyChar
            return (s, rest)


canonicalSExpr :: (Atom (Raw s)) => CharParser () (SExpr [] s)
canonicalSExpr = parseSExprAs fromRaw fromCanonical

advancedSExpr :: (Atom (Simple s)) => CharParser () (SExpr [] s)
advancedSExpr = parseSExprAs fromSimple id

--  I prefer the derived 'Show' instance, for consistency and for 
--  transparency (that is, when I work with 'SExpr's in GHCi I prefer
--  to see their haskelly algebraic structure, not their lispy rendered 
--  form), but this is possible too:
--
-- instance (Atom (Simple a), 
--           List l, Functor l) => 
--          Show (SExpr l a) where
--     showsPrec p = showSExprAs Simple id

-------------------------------------------
-- Public convenience functions: Parsing --
-------------------------------------------

readCanonicalSExprString :: (Atom (Raw s)) => String -> SExpr [] s
readCanonicalSExprString str = 
    case runParser canonicalSExpr () "<readCanonicalSExprString>" str of
        Left err -> error (show err)
        Right ok -> ok

readSExprString :: (Atom (Simple s)) => String -> SExpr [] s
readSExprString str = 
    case runParser advancedSExpr () "<readSExprString>" str of
        Left err -> error (show err)
        Right ok -> ok

readSExpr :: Read a => String -> SExpr [] a
readSExpr = fmap read . readSExprString

-- instance (Atom (Simple a), 
--           List l, Functor l) => 
--          Read (SExpr l a) where
--     readsPrec p = readSExprAs fromSimple id

----------------------------------------------
-- Public convenience functions: Formatting --
----------------------------------------------

showSExpr :: (Atom a, List l, Functor l) =>
     SExpr l a -> ShowS
showSExpr = foldSExpr showsAtom showsList

showSExprAs :: (Atom a2, List l2, Functor l2, Functor l1) =>
     (a1 -> a2) -> (forall t. l1 t -> l2 t)
     -> SExpr l1 a1 -> ShowS
showSExprAs a l = foldSExpr (showsAtom . a) (showsList . l)

printSExpr :: (Atom a, List l, Functor l) =>
     SExpr l a -> Doc
printSExpr = foldSExpr printAtom printList

printSExprAs :: (Atom a2, List l2, Functor l2, Functor l1) =>
     (a1 -> a2) -> (forall t. l1 t -> l2 t)
     -> SExpr l1 a1 -> Doc
printSExprAs a l = foldSExpr (printAtom . a) (printList . l)


basic :: (Atom (Raw s)) => SExpr [] s -> Doc
basic = braces . text . B64.encode . canonicalString

basicString :: (Atom (Raw s)) => SExpr [] s -> String
basicString s = render (basic s)

canonical :: (Atom (Raw s)) => SExpr [] s -> ShowS
canonical = showSExprAs Raw Canonical

canonicalString :: (Atom (Raw s)) => SExpr [] s -> String
canonicalString s = canonical s ""

advanced :: (Atom (Simple s), List l) => SExpr l s -> Doc
advanced = printSExprAs Simple id

advancedString :: (Atom (Simple s), List l) => SExpr l s -> String
advancedString = render . advanced

---------------------------------------------------
-- Public convenience functions: Binary Decoding --
---------------------------------------------------

---------------------------------------------------
-- Public convenience functions: Binary Encoding --
---------------------------------------------------

putSExpr :: (Atom a, List l, Functor l) =>
     SExpr l a -> Put
putSExpr = foldSExpr putAtom putList

putSExprAs :: (Atom a2, List l2, Functor l2, Functor l1) =>
     (a1 -> a2) -> (forall t. l1 t -> l2 t)
     -> SExpr l1 a1 -> Put
putSExprAs a l = foldSExpr (putAtom . a) (putList . l)


putCanonical :: (Atom (Raw s)) => SExpr [] s -> Put
putCanonical = putSExprAs Raw Canonical
