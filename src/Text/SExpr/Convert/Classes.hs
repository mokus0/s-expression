{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Text.SExpr.Convert.Classes where

import Text.PrettyPrint (Doc)

import Data.Binary.Put
import qualified Data.ByteString.Lazy.Char8 as BL

import Text.Parsec (ParsecT, Stream)

-- |A type alias analogous to pre-version-3 parsec's CharParser type.  Not
-- entirely equivalent, as it hides a universal quantification over 2 type 
-- variables, but as long as the RankNTypes language extension is in effect,
-- it's "close enough".
type CharParser st a = (Stream s m Char) => ParsecT s st m a

------------------------
-- Classes for tokens --
------------------------

-- |A type class for parseable and printable atom types.  The 'showsAtom'
-- and 'putAtom' functions are provided to allow different formatting
-- depending on output medium, but by default are implemented in terms of
-- 'printAtom'.  
--
-- Minimum instance definition: 'printAtom' and 'parseAtom'.
class Atom a where
    -- |Print the atom using a ShowS combinator.  Primarily used for rendering
    -- the canonical format.  Should favor compact output.
    --
    -- Default implementation: @showsAtom = shows . printAtom@
    showsAtom :: a -> ShowS
    showsAtom = shows . printAtom
    
    -- |Format the atom as a 'Text.PrettyPrint.Doc'.  Should produce output
    -- formatted nicely for humans to read.
    printAtom :: a -> Doc
    
    -- |Dump the atom to a binary stream, using 'Data.Binary.Put'.  Should
    -- favor compact output.
    --
    -- Default implementation makes use of 'showsAtom'
    putAtom :: a -> Put
    putAtom = putLazyByteString . BL.pack . showAtom
        where showAtom s = showsAtom s ""
    
    -- |A Parsec parser for the atom.  Should accept all of the formats
    -- produced by the instance's other 3 functions.
    parseAtom :: CharParser st a

-- |A type class for parseable and printable list types.
class Functor l => List l where
    -- |Given a list of 'ShowS' combinators representing the list's elements,
    -- produce a single 'ShowS' combinator for the whole list.
    showsList :: l ShowS -> ShowS
    -- |Given a list of 'Doc's representing the list's elements,
    -- produce a single 'Doc' for the whole list.
    printList :: l Doc -> Doc
    -- |Given a list of 'Put' combinators representing the list's elements,
    -- produce a single 'Put' combinator for the whole list.
    putList   :: l Put -> Put
    -- |Given a parser for list elements (sub-s-expressions), parse a list.
    parseList :: CharParser st x -> CharParser st (l x)
