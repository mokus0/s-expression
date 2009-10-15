-- |A Sexpr is an S-expressionin the style of Rivest's Canonical
-- S-expressions.  Atoms may be of any type, but String and
-- ByteString have special support.  Rivest's implementation of
-- S-expressions is unusual in supporting MIME type hints for each
-- atom.  See http://people.csail.mit.edu/rivest/Sexp.txt

module Codec.Sexpr.Internal (-- * Basics
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

import Control.Applicative
import Data.Char
import qualified Data.Foldable as F
import Data.Traversable
import Test.QuickCheck
import Data.Monoid()

data Sexpr s = Atom s
             | HintedAtom String s
             | List [Sexpr s] 

instance Eq s => Eq (Sexpr s) where
    (List a) == (List b) = and $ zipWith (==) a b
    a == b = unAtom a == unAtom b && hint a == hint b

-- |The 'Functor' instance maps over the underlying atomic contents of
-- the S-expression.  It does not map only over the top-level list,
-- but over the fringe.
instance Functor Sexpr where
    fmap f (Atom s) = Atom (f s)
    fmap f (HintedAtom h s) = HintedAtom h (f s)
    fmap f (List ss) = List $ map (fmap f) ss

instance F.Foldable Sexpr where
    foldMap f (Atom s) = f s
    foldMap f (HintedAtom _ s) = f s
    foldMap f (List ss) = F.foldMap (F.foldMap f) ss

instance Traversable Sexpr where
    traverse f (Atom s) = Atom <$> f s
    traverse f (HintedAtom h s) = HintedAtom h <$> f s
    traverse f (List ss) = List <$> traverse (traverse f) ss

instance Arbitrary a => Arbitrary (Sexpr a) where
    arbitrary = sized arbSexpr
        where 
          arbSexpr 0 = oneof [Atom <$> arbitrary,
                              return $ List []]
          arbSexpr n = oneof [Atom <$> arbitrary,
                              List <$> (resize (n `div` 2) arbitrary)]
    coarbitrary (Atom s) = variant 0 . coarbitrary s
    coarbitrary (HintedAtom h s) = variant 1 . coarbitrary_h . coarbitrary s
        where coarbitrary_h = 
                foldr (\a b -> variant (ord a) . variant 1 . b) (variant 0) h
    coarbitrary (List ss) = variant 2 . coarbitrary ss

-- |@fold f s@ applies f to each sub-S-expression of s, from each leaf
-- to the root.  @f@ need not preserve the shape of @s@, in contrast
-- to the shape-preserving @Traversable@ instance.
fold :: (Sexpr t -> Sexpr t) -> Sexpr t -> Sexpr t
fold f z@(Atom _) = f z
fold f z@(HintedAtom _ _) = f z
fold f   (List ss) = f . List $ map (fold f) ss

-- |Any atom whose hint is not specified is assumed to be 
-- "text/plain; charset=iso-8859-1".  This is that default value.
defaultHint :: String
defaultHint = "text/plain; charset=iso-8859-1"

-- |Construct an atom.
atom :: a -> Sexpr a
atom s = Atom s

-- |Construct a list.
list :: [Sexpr a] -> Sexpr a
list xs = List xs

-- |Construct an atom with a MIME type hint.
-- @'hintedAtom' 'defaultHint' == 'atom'@
hintedAtom :: String -> a -> Sexpr a
hintedAtom h s | h == defaultHint = Atom s
hintedAtom h s = HintedAtom h s

-- |A predicate for recognizing lists.
isList :: Sexpr a -> Bool
isList (List _) = True
isList _ = False

-- |A predicate for identifying atoms, whether or not they have
-- explicit hints.
isAtom :: Sexpr a -> Bool
isAtom (List _) = False
isAtom _ = True

-- |Extract the hint of an atom.  Lists do not have hints, but all
-- atoms have hints.
hint :: Sexpr a -> Maybe String
hint (Atom _) = Just defaultHint
hint (HintedAtom h _) = Just h
hint _ = Nothing

-- |Extract the content of an atom, discarding any MIME type hint.
unAtom :: Sexpr s -> s
unAtom (Atom s) = s
unAtom (HintedAtom _ s) = s
unAtom _ = error "unAtom called on a non-atom"

-- |Extract the sub-S-expressions of a List.  If all you intend to do
-- is traverse or map over that list, the Functor instance of
-- S-expressions may work just fine.
unList :: Sexpr s -> [Sexpr s]
unList (List xs) = xs
unList _ = error "unList called on a non-list"

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
