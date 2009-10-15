-- |A SExpr is an S-expressionin the style of Rivest's Canonical
-- S-expressions.  Atoms may be of any type, but String and
-- ByteString have special support.  Similarly, lists may be of any
-- type, but haskell lists have special support. 
-- Rivest's implementation of S-expressions is unusual in supporting 
-- MIME type hints for each atom.  See http:\/\/people.csail.mit.edu\/rivest\/Sexp.txt.
-- This capability is provided by the 'Hinted' type, which wraps atom types
-- and provides the necessary extension to the S-expression syntax.
-- 
-- The "Text.SExpr.Convert" module defines a modular parsing and
-- serializing framework based on two type classes, 'Atom' and 'List'.
-- The special support for [], String and ByteString is implemented via these
-- classes and may be extended to other types by declaring appropriate instances.

module Text.SExpr 
    (   -- * Basics
            SExpr, Hinted,
            Atom(..), Raw, Simple,
            List(..),
            atom,
            isAtom,
            unAtom,
            fromAtom,
            list,
            isList,
            unList,
            fromList,
        -- * Hinted Atoms
            hinted,
            unhinted,
            fromHinted,
            hint,
            maybeHint,
            hintWithDefault,
            dropHint,
            mapHint,
            defaultHint,
        -- * Character predicates to support encoding
            isTokenChar,isInitialTokenChar,isQuoteableChar,
        -- * Transformations
            matchSExpr,
            foldSExpr,
            lmap,
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
            putCanonical,
        -- * Parsers
            readSExpr,
            readSExprString,
            readCanonicalSExprString,
            advancedSExpr,
            canonicalSExpr
    ) where

import Text.SExpr.Type
import Text.SExpr.Convert
