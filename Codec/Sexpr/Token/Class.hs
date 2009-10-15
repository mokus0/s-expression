{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Codec.Sexpr.Token.Class where

import Codec.Sexpr.Type
import Text.Parsec
import Text.PrettyPrint
import Control.Applicative ((<$>))

import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString.Lazy.Char8 as B

----------------------
-- Handy type alias --
----------------------

type CharParser a = (Stream s m Char) => ParsecT s u m a

------------------------
-- Classes for tokens --
------------------------

class AtomToken a where
    showsAtom :: a -> ShowS
    showsAtom = shows . printAtom
    
    printAtom :: a -> Doc
    parseAtom :: CharParser a
    
    putAtom :: a -> Put
    putAtom = putLazyByteString . B.pack . showAtom
        where showAtom s = showsAtom s ""

class ListToken l where
    showsList :: l ShowS -> ShowS
    printList :: l Doc -> Doc
    putList   :: l Put -> Put
    parseList :: CharParser x -> CharParser (l x)

----------------------------------
-- Public convenience functions --
----------------------------------

-- showing
showSExpr :: (AtomToken a, ListToken l, Functor l) =>
     SExpr l a -> ShowS
showSExpr = foldSExpr showsAtom showsList

showSExprAs :: (AtomToken a2, ListToken l2, Functor l2, Functor l1) =>
     (a1 -> a2) -> (forall t. l1 t -> l2 t)
     -> SExpr l1 a1 -> ShowS
showSExprAs a l = showSExpr . foldSExpr (Atom . a) (List . l)

-- pretty-printing
printSExpr :: (AtomToken a, ListToken l, Functor l) =>
     SExpr l a -> Doc
printSExpr = foldSExpr printAtom printList

printSExprAs :: (AtomToken a2, ListToken l2, Functor l2, Functor l1) =>
     (a1 -> a2) -> (forall t. l1 t -> l2 t)
     -> SExpr l1 a1 -> Doc
printSExprAs a l = printSExpr . foldSExpr (Atom . a) (List . l)

-- binary encoding
putSExpr :: (AtomToken a, ListToken l, Functor l) =>
     SExpr l a -> Put
putSExpr = foldSExpr putAtom putList

putSExprAs :: (AtomToken a2, ListToken l2, Functor l2, Functor l1) =>
     (a1 -> a2) -> (forall t. l1 t -> l2 t)
     -> SExpr l1 a1 -> Put
putSExprAs a l = putSExpr . foldSExpr (Atom . a) (List . l)

-- parsing
sexpr :: (AtomToken a, ListToken l) => CharParser (SExpr l a)
sexpr = (Atom <$> parseAtom) 
    <|> (List <$> parseList sexpr)

parseSExpr :: (AtomToken a, ListToken l) => CharParser (SExpr l a)
parseSExpr = do
    s <- sexpr
    eof
    return s

parseSExprAs
  :: (Functor l1, AtomToken a1, ListToken l1) =>
     (a1 -> a2)
     -> (l1 (SExpr l2 a2) -> l2 (SExpr l2 a2))
     -> CharParser (SExpr l2 a2)
parseSExprAs a l = foldSExpr (Atom . a) (List . l) <$> parseSExpr

-- reading
readSExpr :: (AtomToken a, ListToken l) => ReadS (SExpr l a)
readSExpr str = case runParser parse () "<readSExpr>" str of
    Left err -> []
    Right x  -> [x]
    where 
        parse = do
            s <- sexpr
            rest <- many anyChar
            return (s, rest)

readSExprAs
  :: (Functor l1, AtomToken a1, ListToken l1) =>
     (a1 -> a2)
     -> (l1 (SExpr l2 a2) -> l2 (SExpr l2 a2))
     -> ReadS (SExpr l2 a2)
readSExprAs a l str = [(foldSExpr (Atom . a) (List . l) x, rest) | (x,rest) <- readSExpr str]
    
