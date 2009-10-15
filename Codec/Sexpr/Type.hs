{-# LANGUAGE
        StandaloneDeriving, FlexibleContexts, UndecidableInstances,
        FlexibleInstances,
        DeriveDataTypeable,
        RankNTypes
  #-}

module Codec.Sexpr.Type where

import Control.Applicative
import Control.Monad
import Data.Foldable (Foldable, foldMap)
import qualified Data.Foldable as F
import Data.Traversable (Traversable, sequenceA, traverse)
import Data.Generics (Data, Typeable, Typeable1(..), mkTyCon, mkTyConApp)
import Test.QuickCheck (Arbitrary(..), variant, choose, oneof, sized, resize, frequency)
import Data.Char (ord, isAlpha, isAlphaNum, isSpace)

-- |An s-expression consists of atoms and lists of s-expressions.  The atom type 
-- and the list type are given as parameters here, because there are many useful
-- implementations, as well as many useful variations that do not model classical
-- s-expressions at all.  
-- 
-- Examples of the former include versions of lists and atoms that retain 
-- whitespace and comments from the original source.
-- 
-- Examples of the latter include  intermediate stages of tree transformations
-- from s-expressions to other structures (see the property-list package source
-- for an example of a similar pattern in the use of the PropertyList type, 
-- where XML transformation is effected by
-- Monad operations).
data SExpr l a
    = Atom a
    | List (l (SExpr l a))

atom :: a -> SExpr l a
atom = Atom

unAtom :: SExpr l a -> Maybe a
unAtom (Atom a) = Just a
unAtom _        = Nothing

list :: l (SExpr l a) -> SExpr l a
list = List

unList :: SExpr l a -> Maybe (l (SExpr l a))
unList (List l) = Just l
unList _        = Nothing

deriving instance (Eq       a, Eq       (l (SExpr l a))) => Eq       (SExpr l a)
deriving instance (Ord      a, Ord      (l (SExpr l a))) => Ord      (SExpr l a)
deriving instance (Show     a, Show     (l (SExpr l a))) => Show     (SExpr l a)

instance Typeable1 l => Typeable1 (SExpr l) where
 -- typeOf1 :: (SExpr l) a -> TypeRep {- of "SExpr l" -}
    typeOf1 x = mkTyConApp (mkTyCon "Codec.FedFile.SExpr") [typeOf1 (toL x)]
        where
            toL :: SExpr l a -> l a
            toL _ = error "typeOf1 evaluated its argument! oh, the humanity!"

deriving instance (Typeable1 l, Data a, Data (l (SExpr l a))) => Data (SExpr l a)
        
instance Functor l => Functor (SExpr l) where
    fmap f (Atom  x) = Atom (f x)
    fmap f (List xs) = List (fmap (fmap f) xs)

instance Functor l => Monad (SExpr l) where
    return = Atom
    Atom  x >>= f   = f x
    List xs >>= f   = List (fmap (>>= f) xs)

instance Functor l => Applicative (SExpr l) where
    pure = return
    (<*>) = ap

instance Foldable l => Foldable (SExpr l) where
    foldMap f (Atom  x) = f x
    foldMap f (List xs) = foldMap (foldMap f) xs

instance Traversable l => Traversable (SExpr l) where
    sequenceA  (Atom  x) = fmap Atom x
    sequenceA  (List xs) = fmap List (traverse sequenceA xs)
    traverse f (Atom  x) = fmap Atom (f x)
    traverse f (List xs) = fmap List (traverse (traverse f) xs)

instance (Arbitrary a, Arbitrary (l (SExpr l a))) => Arbitrary (SExpr l a) where
    arbitrary = oneof [Atom <$> arbitrary,
                       List <$> sized arbList]
        where 
            arbList sz  = resize (sz `div` 2) arbitrary
              
    coarbitrary (Atom a) = variant 0 . coarbitrary a
    coarbitrary (List l) = variant 1 . coarbitrary l


-- |Examine the outermost constructor of the s-expression and pass its
-- contents to either the first function (in case of an atom) or the second
-- (in case of a list).  Effectively the same as a pattern match, but 
-- independent of the actual implementation of the SExpr type, should that
-- implementation change in the future.
matchSExpr :: (a -> b) -> (l (SExpr l a) -> b) -> SExpr l a -> b
matchSExpr a _ (Atom  x) = a x
matchSExpr _ l (List xs) = l xs

-- |Fold an s-expression; ie, substitute the given functions for each of
-- the data constructors.
foldSExpr :: (Functor l) => (a -> b) -> (l b -> b) -> SExpr l a -> b
foldSExpr = foldSExprBy fmap

-- |Fold an s-expression with additional control over the recursion through
-- lists (@foldSExprBy fmap atom list == fix (\self -> matchSExpr atom (list . fmap self))@).
-- The parameter name @fmap@ is deliberately chosen to illuminate its nature.
-- The 'default' case (the one used in 'foldSExpr') is 'fmap' from the 'Functor' 
-- class.
foldSExprBy :: ((SExpr l a -> c) -> l (SExpr l a) -> b)
     -> (a -> c) -> (b -> c) -> SExpr l a -> c
foldSExprBy fm a l = go
    where go = matchSExpr a (l . fm go)

-- |systematically replace the list structures in a s-expression
lmap :: Functor l1 => (l1 (SExpr l2 a) -> l2 (SExpr l2 a)) -> SExpr l1 a -> SExpr l2 a
lmap f = foldSExpr Atom (List . f)

-- |systematically replace the list structures in a s-expression by means of a
-- natural transformation from one list to the other (ie., the mapping does not
-- depend in any way on the contents of each list)
lmapNat :: Functor l1 => (forall t. l1 t -> l2 t) -> SExpr l1 a -> SExpr l2 a
lmapNat f = foldSExpr Atom (List . f)

-- |Rather than representing display hints at the 'SExpr' structure level, they
-- are represented as tags on the atoms.  Thus, an 'SExpr' with hints will
-- be represented by a type such as @SExpr [] (Hinted String)@.
data Hinted h a
    = Hinted h a
    | UnHinted a
    deriving (Eq, Ord, Show, Typeable, Data)

instance Functor (Hinted h) where
    fmap f (Hinted h x) = Hinted h (f x)
    fmap f (UnHinted x) = UnHinted (f x)
instance Foldable (Hinted h) where
    foldMap f (Hinted _ x) = f x
    foldMap f (UnHinted x) = f x
instance Traversable (Hinted h) where
    sequenceA (Hinted h x) = fmap (Hinted h) x
    sequenceA (UnHinted x) = fmap UnHinted   x
instance Arbitrary a => Arbitrary (Hinted String a) where
    arbitrary = oneof [arbHinted, arbUnhinted]
        where 
            arbHintChar = frequency 
                [ (26, choose ('a','z'))
                , (26, choose ('A','Z'))
                , (10, choose ('0','9'))
                , (1,  return ' ')
                ]
            arbHint = sized $ \sz -> replicateM sz arbHintChar
            arbHinted = sized $ \sz -> do
                hsz <- choose (0,sz)
                h <- resize hsz arbHint
                x <- resize (sz - hsz) arbitrary
                return (Hinted h x)
            arbUnhinted = UnHinted <$> arbitrary
    coarbitrary (UnHinted x) = variant 0 . coarbitrary x
    coarbitrary (Hinted h x) = variant 1 . coarbitrary x . coarbitrary_h
        where 
            coarbitrary_h = foldr (\a b -> variant (ord a) . variant 1 . b) (variant 0) h


-- |Any atom whose hint is not specified is assumed to be 
-- "text/plain; charset=iso-8859-1".  This is that default value.
defaultHint :: String
defaultHint = "text/plain; charset=iso-8859-1"

hint :: Hinted String a -> String
hint (Hinted h _) = h
hint (UnHinted _) = defaultHint

-- |Discard the hint from an atom, or with 'fmap', from a whole 'SExpr' (eg,
-- @fmap unHint :: SExpr l (Hinted a) -> SExpr l a@)
unHint :: Hinted h a -> a
unHint (Hinted _ x) = x
unHint (UnHinted x) = x

mapHint :: (a -> x) -> (b -> y) -> Hinted a b -> Hinted x y
mapHint f g (Hinted h x) = Hinted (f h) (g x)
mapHint _ g (UnHinted x) = UnHinted     (g x)

---------------------------------------
-- Character predicates for encoding --
---------------------------------------

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

