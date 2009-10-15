{-# LANGUAGE
        StandaloneDeriving
  #-}

-- |Compatibility module for sexpr-0.2.1 interface.

module Codec.Sexpr.Compat.Internal
    (-- * Basics
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
     defaultHint,
     -- * Character predicates to support encoding
     isTokenChar,isInitialTokenChar,isQuoteableChar,
     -- * Transformations
     fold
    ) where

import Codec.Sexpr.Type (SExpr(..), Hinted(..))
import qualified Codec.Sexpr.Type as Type
import Control.Applicative ((<$>))
import Data.Foldable (Foldable(foldMap))
import Data.Traversable (Traversable(..))
import Test.QuickCheck (Arbitrary(..), sized, oneof, variant, resize)
import Data.Char (ord, isAlpha, isAlphaNum, isSpace)

newtype Sexpr s = Sexpr { unSexpr :: SExpr [] (Hinted String s) }

deriving instance (Eq s) => Eq (Sexpr s)

instance Functor Sexpr where
    fmap f (Sexpr s) = Sexpr (fmap (fmap f) s)

instance Foldable Sexpr where
    foldMap f (Sexpr s) = foldMap (foldMap f) s

instance Traversable Sexpr where
    sequenceA (Sexpr s) = Sexpr <$> traverse sequenceA s

instance (Arbitrary a) => Arbitrary (Sexpr a) where
    arbitrary = Sexpr <$> arbitrary
    coarbitrary (Sexpr (Atom (UnHinted s))) = variant 0 . coarbitrary s
    coarbitrary (Sexpr (Atom (Hinted h s))) = variant 1 . coarbitrary_h . coarbitrary s
        where coarbitrary_h = 
                foldr (\a b -> variant (ord a) . variant 1 . b) (variant 0) h
    coarbitrary (Sexpr (List ss)) = variant 2 . coarbitrary (Sexpr <$> ss)

fold :: (Sexpr t -> Sexpr t) -> Sexpr t -> Sexpr t
fold = undefined

defaultHint :: String
defaultHint = Type.defaultHint

atom :: a -> Sexpr a
atom = Sexpr . Atom . UnHinted

list :: [Sexpr a] -> Sexpr a
list = Sexpr . List . map unSexpr

hintedAtom :: String -> a -> Sexpr a
hintedAtom h = Sexpr . Atom . Hinted h

isList :: Sexpr a -> Bool
isList (Sexpr (List _)) = True
isList _                = False

isAtom :: Sexpr a -> Bool
isAtom (Sexpr (Atom _)) = True
isAtom _                = False

hint :: Sexpr a -> Maybe String
hint (Sexpr (Atom a))   = Just (Type.hint a)
hint _                  = Nothing

unAtom :: Sexpr s -> s
unAtom (Sexpr (Atom a)) = Type.unHint a
unAtom _                = error "unAtom called on a non-atom"

unList :: Sexpr s -> [Sexpr s]
unList (Sexpr (List l)) = map Sexpr l
unList _                = error "unList called on a non-list"

-- |Tokens may begin with any alphabetic character or the characters
-- in @"-./_:*+="@ ;
isInitialTokenChar :: Char -> Bool
isInitialTokenChar x = (isAlpha x || x `elem` "-./_:*+=") && (128 > ord x)

-- |Tokens may internally contain any of the characters legitimate to
-- begin tokens, or any numeral.
isTokenChar :: Char -> Bool
isTokenChar x = (isAlphaNum x || x `elem` "-./_:*+=") && (128 > ord x)

-- |Only token characters and spaces don't need to be escaped when
-- shown in the "quoted" syntax.
isQuoteableChar :: Char -> Bool
isQuoteableChar x = isTokenChar x || isSpace x
