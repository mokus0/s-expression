-- |Compatibility module for sexpr-0.2.1 interface.

module Codec.Sexpr.Compat (-- * Basics
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

import Codec.Sexpr.Compat.Internal
import Codec.Sexpr.Compat.Parser
import Codec.Sexpr.Compat.Printer
