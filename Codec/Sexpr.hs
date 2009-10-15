-- |A Sexpr is an S-expressionin the style of Rivest's Canonical
-- S-expressions.  Atoms may be of any type, but String and
-- ByteString have special support.  Rivest's implementation of
-- S-expressions is unusual in supporting MIME type hints for each
-- atom.  See http://people.csail.mit.edu/rivest/Sexp.txt

module Codec.Sexpr (-- * Basics
                             SExpr,
                             atom,
                             list,
                             unAtom,
                             unList,
                             -- * Hinted Atoms
                             hint,
                             unHint,
                             defaultHint,
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
                             readSexpr,
                             readSexprString,
                             readCanonicalSexprString,
                             advancedSexpr,
                             canonicalSexpr
                             ) where

import Codec.Sexpr.Type
import Codec.Sexpr.Token
import Codec.Sexpr.Parser
import Codec.Sexpr.Printer
