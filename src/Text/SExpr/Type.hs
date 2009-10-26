{-# LANGUAGE
        StandaloneDeriving, FlexibleContexts, UndecidableInstances,
        FlexibleInstances,
        DeriveDataTypeable,
        RankNTypes
  #-}

module Text.SExpr.Type where

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
-- whitespace and comments from the original source, or lists and/or atoms
-- containing @IORef@s or other mutable references.
-- 
-- Examples of the latter include  intermediate stages of tree transformations
-- from s-expressions to other structures (see the property-list package source
-- for an example of a similar pattern in the use of the PropertyList type, 
-- where XML transformation is effected by 'Monad' operations).

data SExpr l a
    = Atom a
    | List (l (SExpr l a))

-- |Construct an atom.
atom :: a -> SExpr l a
atom = Atom

-- |A predicate for identifying atoms.
isAtom :: SExpr l a -> Bool
isAtom (Atom _) = True
isAtom _        = False

-- |Extract an atom.  This function is partial; it throws an error on non-atoms.
unAtom :: SExpr l a -> a
unAtom (Atom a) = a
unAtom _        = error "unAtom called on a non-atom"

-- |Extract an atom.  This function is total; it returns 'Nothing' on non-atoms.
fromAtom :: SExpr l a -> Maybe a
fromAtom (Atom a) = Just a
fromAtom _        = Nothing

-- |Construct a list.
list :: l (SExpr l a) -> SExpr l a
list = List

-- |A predicate for recognizing lists.
isList :: SExpr l a -> Bool
isList (List _) = True
isList _        = False

-- |Extract a list.  This function is partial; it throws an error on non-lists.
unList :: SExpr l a -> l (SExpr l a)
unList (List a) = a
unList _        = error "unList called on a non-list"

-- |Extract a list.  This function is total; it return 'Nothing' on non-lists.
fromList :: SExpr l a -> Maybe (l (SExpr l a))
fromList (List l) = Just l
fromList _        = Nothing

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
{-# RULES
"matchSExpr/id" matchSExpr Atom List = id
  #-}
matchSExpr :: (a -> b) -> (l (SExpr l a) -> b) -> SExpr l a -> b
matchSExpr a _ (Atom  x) = a x
matchSExpr _ l (List xs) = l xs

-- |Simultaneously map over the lists and the atoms of an s-expression without
-- changing its basic structure.
{-# RULES
-- lots of identities, intended to fuse multiple traversals.

-- simplifying identity traversals (rules for fmap and lmap may
-- then further simplify in case of identity in both parameters):
"mapSExpr/id/_"               mapSExpr id   = lmap
"mapSExpr/_/id"     forall f. mapSExpr f id = fmap f

-- Absorbing 'lmap's:
"mapSExpr.lmap"     forall f g h.   mapSExpr f g . lmap h    = mapSExpr f (g.h)
"mapSExpr.lmap"     forall f g h x. mapSExpr f g  (lmap h x) = mapSExpr f (g.h) x
"lmap.mapSExpr"     forall f g h.   lmap g . mapSExpr f h    = mapSExpr f (g.h)
"lmap.mapSExpr"     forall f g h x. lmap g  (mapSExpr f h x) = mapSExpr f (g.h) x

-- Absorbing 'fmap's:
"mapSExpr.fmap"     forall f g h.   mapSExpr f g . fmap h    = mapSExpr (f.h) g
"mapSExpr.fmap"     forall f g h x. mapSExpr f g  (fmap h x) = mapSExpr (f.h) g x
"fmap.mapSExpr"     forall f g h.   fmap f . mapSExpr g h    = mapSExpr (f.g) h
"fmap.mapSExpr"     forall f g h x. fmap f  (mapSExpr g h x) = mapSExpr (f.g) h x

-- Fusing double 'mapSExpr's:
"mapSExpr.mapSExpr" forall f1 g1 f2 g2.   mapSExpr f1 g1 . mapSExpr f2 g2    = mapSExpr (f1 . f2) (g1 . g2)
"mapSExpr.mapSExpr" forall f1 g1 f2 g2 x. mapSExpr f1 g1  (mapSExpr f2 g2 x) = mapSExpr (f1 . f2) (g1 . g2) x
  #-}
{-# NOINLINE mapSExpr #-}
mapSExpr :: Functor l1 => (a1 -> a2) -> (l1 (SExpr l2 a2) -> l2 (SExpr l2 a2)) -> SExpr l1 a1 -> SExpr l2 a2
mapSExpr f _ (Atom  x) = Atom (f x)
mapSExpr f g (List xs) = List (g (fmap (mapSExpr f g) xs))

-- |Fold an s-expression; ie, substitute the given functions for each of
-- the data constructors.  For example, using types from the data-object 
-- package:
-- 
-- > sexprToObject :: SExpr [] a -> GenObject key a
-- > sexprToObject = foldSExpr Scalar Sequence
--
-- or, slightly less trivial:
-- 
-- > isKVPair (Sequence (Scalar k:val)) = not (null val)
-- > isKVPair _ = False
-- > 
-- > toKVPair (Sequence [Scalar k,val]) = (k, val)
-- > toKVPair (Sequence (Scalar k:val)) = (k, toObj val)
-- > 
-- > toObj xs | all isKVPair xs = Mapping (map toKVPair xs) 
-- >          | otherwise       = Sequence xs
-- > 
-- > sexprToObject x = foldSExpr Scalar toObj (list [x])
{-# RULES
-- Absorbing 'fmap's:
"foldSExpr/fmap"    forall l a f.   foldSExpr l a . fmap f    = foldSExpr (l . f) a
"foldSExpr/fmap"    forall l a f x. foldSExpr l a  (fmap f x) = foldSExpr (l . f) a x

-- Absorbing 'lmap's:
"foldSExpr/lmap"    forall l a f.   foldSExpr l a . lmap f    = foldSExpr l (a . f)
"foldSExpr/lmap"    forall l a f x. foldSExpr l a  (lmap f x) = foldSExpr l (a . f) x

-- Absorbing 'mapSExpr's:
"foldSExpr/mapSExpr"    forall l a f g.   foldSExpr l a . mapSExpr f g    = foldSExpr (l . f) (a . g)
"foldSExpr/mapSExpr"    forall l a f g x. foldSExpr l a  (mapSExpr f g x) = foldSExpr (l . f) (a . g) x
  #-}
{-# NOINLINE foldSExpr #-}
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

-- |systematically replace the list structures in an s-expression
{-# RULES
-- Eliminating identity traversals:
"lmap/id"       lmap id = id

-- Combining multiple 'lmap' traversals:
"lmap.lmap"     forall f g.   lmap f . lmap g    = lmap (f.g)
"lmap.lmap"     forall f g x. lmap f  (lmap g x) = lmap (f.g) x

  #-}
lmap :: Functor l1 => (l1 (SExpr l2 a) -> l2 (SExpr l2 a)) -> SExpr l1 a -> SExpr l2 a
lmap f = foldSExpr Atom (List . f)

-- |systematically replace the list structures in an s-expression by means of a
-- natural transformation from one list to the other (ie., the mapping does not
-- depend in any way on the contents of each list)
{-# RULES
-- after it passes initial typechecking, 'lmapNat' is identical to 'lmap'.  To
-- simplify RULES processing, replace it with 'lmap' (weakening the type) so
-- that all 'lmap' rules apply.
"lmap: weakening"   forall (f :: forall t. l1 t -> l2 t) . lmapNat f = lmap f
  #-}
lmapNat :: Functor l1 => (forall t. l1 t -> l2 t) -> SExpr l1 a -> SExpr l2 a
lmapNat f = foldSExpr Atom (List . f)

-- |Rather than representing display hints at the 'SExpr' structure level, they
-- are represented as tags on the atoms.  Thus, an 'SExpr' with 'String' hints
-- would be represented by a type such as @SExpr [] (Hinted String a)@.
data Hinted h a
    = Hinted h a
    | Unhinted a
    deriving (Eq, Ord, Show, Typeable, Data)

{-# RULES
-- Simplify identities:
"fromHinted.hinted"     forall h x. fromHinted (hinted h x) = (Just h,  x)
"fromHinted.unhinted"   forall   x. fromHinted (unhinted x) = (Nothing, x)

"hint.hinted"           forall h x. hint  (hinted h x) = h
"hint.hinted"           forall h.   hint . hinted h    = const h
"hint.unhinted"         forall   x. hint  (unhinted x) = defaultHint
"hint.unhinted"                     hint . unhinted    = const defaultHint

"hint.hinted"           forall d h x. hintWithDefault d  (hinted h x) = h
"hint.hinted"           forall d h.   hintWithDefault d . hinted h    = const h
"hint.unhinted"         forall d   x. hintWithDefault d  (unhinted x) = d
"hint.unhinted"         forall d.     hintWithDefault d . unhinted    = const d

"dropHint.hinted"       forall h x. dropHint  (hinted h x) = x
"dropHint.hinted"       forall h.   dropHint . hinted h    = id
"dropHint.unhinted"     forall   x. dropHint  (unhinted x) = x
"dropHint.unhinted"                 dropHint . unhinted    = id
  #-}

-- |Construct a hinted atom with a MIME type hint.
hinted :: h -> a -> Hinted h a
hinted = Hinted

-- |Construct a hinted atom without a MIME type hint.
unhinted :: a -> Hinted h a
unhinted = Unhinted

-- |Deconstruct a hinted atom.
fromHinted :: Hinted h a -> (Maybe h, a)
fromHinted (Hinted h a) = (Just h,  a)
fromHinted (Unhinted a) = (Nothing, a)

instance Functor (Hinted h) where
    fmap f (Hinted h x) = Hinted h (f x)
    fmap f (Unhinted x) = Unhinted (f x)
instance Foldable (Hinted h) where
    foldMap f (Hinted _ x) = f x
    foldMap f (Unhinted x) = f x
instance Traversable (Hinted h) where
    sequenceA (Hinted h x) = fmap (Hinted h) x
    sequenceA (Unhinted x) = fmap Unhinted   x
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
            arbUnhinted = Unhinted <$> arbitrary
    coarbitrary (Unhinted x) = variant 0 . coarbitrary x
    coarbitrary (Hinted h x) = variant 1 . coarbitrary x . coarbitrary_h
        where 
            coarbitrary_h = foldr (\a b -> variant (ord a) . variant 1 . b) (variant 0) h


-- |Any atom whose hint is not specified is assumed to be 
-- \"text/plain; charset=iso-8859-1\".  This is that default value.
defaultHint :: String
defaultHint = "text/plain; charset=iso-8859-1"

-- |Extract the hint of a 'Hinted' atom.
hint :: Hinted String a -> String
hint = hintWithDefault defaultHint

-- |Extract the hint of a 'Hinted' atom if one is explicitly given.
maybeHint :: Hinted h s -> Maybe h
maybeHint (Hinted h _) = Just h
maybeHint (Unhinted _) = Nothing

-- |Extract the hint of a 'Hinted' atom.
hintWithDefault :: h -> Hinted h s -> h
hintWithDefault _ (Hinted h _) = h
hintWithDefault d (Unhinted _) = d

-- |Discard the hint from an atom, or with 'fmap', from a whole 'SExpr' (eg,
-- @fmap unHint :: SExpr l (Hinted a) -> SExpr l a@)
dropHint :: Hinted h a -> a
dropHint (Hinted _ x) = x
dropHint (Unhinted x) = x

-- |Alter the hint and content of a 'Hinted' value.
-- @mapHint f g@ applies @f@ to the hint and @g@ to the value.
{-# RULES
-- simplify identity traversals:
"mapHint/id"    mapHint id = fmap
  #-}
mapHint :: (a -> x) -> (b -> y) -> Hinted a b -> Hinted x y
mapHint f g (Hinted h x) = Hinted (f h) (g x)
mapHint _ g (Unhinted x) = Unhinted     (g x)

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

