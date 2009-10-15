{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, UndecidableInstances #-}
-- |A Sexpr is an S-expressionin the style of Rivest's Canonical
-- S-expressions.  Atoms may be of any type, but String and
-- ByteString have special support.  Rivest's implementation of
-- S-expressions is unusual in supporting MIME type hints for each
-- atom.  See http://people.csail.mit.edu/rivest/Sexp.txt
--
-- This module is a wrapper for compatibility with verion 0.2.1 
-- of the sexpr package.
module Codec.Sexpr 
    (   -- * Basics
            Sexpr,
            isAtom,
            isList,
            atom,
            list,
            unAtom,
            unList,
        -- * Hinted Atoms
            hintedAtom,
            hint,
            S.defaultHint,
        -- * Character predicates to support encoding
            S.isTokenChar,S.isInitialTokenChar,S.isQuoteableChar,
        -- * Transformations
            fold,
        -- * String printers
            canonicalString,
            basicString,
            advancedString,
        -- * ShowS printers
            canonical,
        -- * Doc pretty printers
            basic,
            advanced,
        -- * Put binary printers
            putCanonical, putCanonicalBS,
        -- * Parsers
            readSexpr,
            readSexprString,
            readCanonicalSexprString,
            advancedSexpr,
            canonicalSexpr
    ) where

import Text.SExpr.Type (SExpr(..), Hinted(..))
import qualified Text.SExpr.Type as S
import qualified Text.SExpr.Convert as S

import Control.Applicative ((<$>))
import Data.Traversable (Traversable(sequenceA, traverse))
import Data.Foldable    (Foldable(foldMap))
import Test.QuickCheck  (Arbitrary)

import Text.ParserCombinators.ReadP (ReadP, readS_to_P)
import Data.ByteString (ByteString)
import Text.PrettyPrint (Doc)
import Data.Binary (Put)

newtype Sexpr a = Sexpr { unSexpr :: (SExpr [] (Hinted String a)) }
    deriving (Eq, Arbitrary)

instance Functor Sexpr where
    fmap f (Sexpr s) = Sexpr (fmap (fmap f) s)

instance Foldable Sexpr where
    foldMap f (Sexpr s) = foldMap (foldMap f) s

instance Traversable Sexpr where
    sequenceA (Sexpr s) = Sexpr <$> traverse sequenceA s

instance Read s => Read (Sexpr s) where
    readsPrec _ str = [ (Sexpr sexp, rest)| (sexp, rest) <- S.readsSExprAs (S.mapHint id read . S.fromSimple) id str]

instance Show s => Show (Sexpr s) where
    showsPrec _ (Sexpr s) = S.showSExprAs (S.Simple . S.mapHint id show) id s

-- |A predicate for identifying atoms, whether or not they have
-- explicit hints.
isAtom :: Sexpr a -> Bool
isAtom = S.isAtom . unSexpr

-- |A predicate for recognizing lists.
isList :: Sexpr a -> Bool
isList = S.isList . unSexpr

-- |Construct an atom.
atom :: a -> Sexpr a
atom = Sexpr . S.atom . S.unhinted

list :: [Sexpr a] -> Sexpr a
list = Sexpr . S.list . fmap unSexpr

-- |Extract the hint of an atom.  Lists do not have hints, but all
-- atoms have hints.
unAtom :: Sexpr s -> s
unAtom = S.dropHint . S.unAtom . unSexpr

-- |Extract the sub-S-expressions of a List.  If all you intend to do
-- is traverse or map over that list, the Functor instance of
-- S-expressions may work just fine.
unList :: Sexpr s -> [Sexpr s]
unList = fmap Sexpr . S.unList . unSexpr

-- |Construct an atom with a MIME type hint.
-- @'hintedAtom' 'defaultHint' == 'atom'@
hintedAtom :: String -> a -> Sexpr a
hintedAtom h = Sexpr . S.atom . S.hinted h

-- |Extract the hint of an atom.  Lists do not have hints, but all
-- atoms have hints.
hint :: Sexpr a -> Maybe String
hint = fmap S.hint . S.fromAtom . unSexpr

-- |@fold f s@ applies f to each sub-S-expression of s, from each leaf
-- to the root.  @f@ need not preserve the shape of @s@, in contrast
-- to the shape-preserving @Traversable@ instance.
fold :: (Sexpr t -> Sexpr t) -> Sexpr t -> Sexpr t
fold f z@(Sexpr (Atom  _)) = f z
fold f   (Sexpr (List ss)) = f . list $ map (fold f . Sexpr) ss

canonicalString :: Sexpr String -> String
canonicalString (Sexpr s) = S.canonicalString s
basicString     :: Sexpr String -> String
basicString (Sexpr s) = S.basicString s
advancedString  :: Sexpr String -> String
advancedString (Sexpr s) = S.advancedString s

canonical :: Sexpr String -> ShowS
canonical (Sexpr s) = S.canonical s

basic :: Sexpr String -> Doc
basic (Sexpr s) = S.basic s

advanced :: Sexpr String -> Doc
advanced (Sexpr s) = S.advanced s

putCanonical :: Sexpr String -> Put
putCanonical (Sexpr s) = S.putCanonical s

putCanonicalBS :: Sexpr ByteString -> Put
putCanonicalBS (Sexpr s) = S.putCanonical s

-- |Read a @'Sexpr' a@ using the 'Read' instance for @a@.  The Sexpr
-- may be in any encoding: Canonical, Basic, or Advanced.
readSexpr :: Read a => String -> Sexpr a
readSexpr = fmap read . readSexprString

-- |Read a @'Sexpr' 'String'@ in any encoding: Canonical, Basic, or Advanced.
readSexprString :: String -> Sexpr String
readSexprString = Sexpr <$> S.readSExprString

-- |Read a @'Sexpr' 'String'@ in canonical encoding.
readCanonicalSexprString :: String -> Sexpr String
readCanonicalSexprString = Sexpr <$> S.readCanonicalSExprString

advancedSexpr :: ReadP (Sexpr String)
advancedSexpr = Sexpr <$> readS_to_P (S.readsSExprAs S.fromSimple id)

-- |For some applications it is wise to accept only very carefully
-- specified input.  This is useful when you know you are receiving
-- exactly a Canonical S-Expression.  It will read only a Canonical
-- S-expression (and optional terminating NUL), but not the Basic or
-- Advanced encodings.
canonicalSexpr :: ReadP (Sexpr String)
canonicalSexpr = Sexpr <$> readS_to_P (S.readsSExprAs S.fromRaw id)
