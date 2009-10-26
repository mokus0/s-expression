{-# LANGUAGE 
        FlexibleInstances, FlexibleContexts,
        GeneralizedNewtypeDeriving
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.SExpr.Convert.Instances where

import Text.SExpr.Type
import Text.SExpr.Convert.Classes
import Text.SExpr.Parse
import Text.SExpr.Print

import Control.Applicative ((<$>))
import Data.List (intersperse)

import Text.PrettyPrint
import Data.Binary
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

import Unsafe.Coerce {- for rewrite rules only -}

{-# RULES
-- ensure that fmap rewrites apply to <$> and map too
"INLINE <$>" (<$>) = fmap

-- various unsafeCoerce rules, triggered indirectly through rules for newtype constructors
-- (purpose is to eliminate unnecessary traversals where possible, caused by fmap-ing
-- newtype constructors)
"unsafeCoerce.unsafeCoerce/1"   unsafeCoerce . unsafeCoerce = unsafeCoerce
"unsafeCoerce.unsafeCoerce/2"   forall x . unsafeCoerce (unsafeCoerce x) = unsafeCoerce x
"fmap/unsafeCoerce"             fmap unsafeCoerce = unsafeCoerce
"map/unsafeCoerce"              map  unsafeCoerce = unsafeCoerce
"fmap.unsafeCoerce"             forall f. fmap f . unsafeCoerce = fmap (f . unsafeCoerce)
"unsafeCoerce.fmap"             forall f. unsafeCoerce . fmap f = fmap (unsafeCoerce . f)
  #-}

-- Hinted atoms.
instance (Atom h, Atom s) => Atom (Hinted h s) where
    showsAtom = showsHinted
    printAtom = printHinted
    putAtom   = putHinted
    parseAtom = hintedAtom

-- |Raw strings for the canonical and basic encodings
newtype Raw s = Raw { fromRaw :: s }
{-# RULES
"Raw"           Raw = unsafeCoerce
"fromRaw"       fromRaw = unsafeCoerce
  #-}
instance Atom (Raw String) where
    showsAtom = raw    . fromRaw
    printAtom = rawDoc . fromRaw
    putAtom   = putRaw . fromRaw
    parseAtom = Raw <$> rawString

instance Atom (Raw B.ByteString) where
    showsAtom = rawBS    . fromRaw
    printAtom = rawDocBS . fromRaw
    putAtom   = putRawBS . fromRaw
    parseAtom = Raw . B.pack <$> rawString

instance Atom (Raw BL.ByteString) where
    showsAtom = rawBSL    . fromRaw
    printAtom = rawDocBSL . fromRaw
    putAtom   = putRawBSL . fromRaw
    parseAtom = Raw . BL.pack <$> rawString

instance (Atom (Raw h), Atom (Raw s)) => Atom (Raw (Hinted h s)) where
    showsAtom = showsAtom . mapHint Raw Raw . fromRaw
    printAtom = printAtom . mapHint Raw Raw . fromRaw
    putAtom   = putAtom   . mapHint Raw Raw . fromRaw
    parseAtom = Raw . mapHint fromRaw fromRaw <$> parseAtom

-- | \<simple-string\>s in the \"advanced\" encoding.  No formatting information is
-- retained when parsing.
newtype Simple s = Simple { fromSimple :: s }
{-# RULES
"Simple"            Simple = unsafeCoerce
"fromSimple"        Simple = unsafeCoerce
  #-}
instance Atom (Simple String) where
    printAtom = format . fromSimple
    parseAtom = Simple <$> simpleString
    
instance Atom (Simple B.ByteString) where
    printAtom = format . B.unpack . fromSimple
    parseAtom = Simple . B.pack <$> simpleString
    
instance Atom (Simple BL.ByteString) where
    printAtom = format . BL.unpack . fromSimple
    parseAtom = Simple . BL.pack <$> simpleString
    
instance (Atom (Simple h), Atom (Simple s)) => Atom (Simple (Hinted h s)) where
    showsAtom = showsAtom . mapHint Simple Simple . fromSimple
    printAtom = printAtom . mapHint Simple Simple . fromSimple
    putAtom   = putAtom   . mapHint Simple Simple . fromSimple
    parseAtom = Simple . mapHint fromSimple fromSimple <$> parseAtom


--------------------------
-- List implementations --
--------------------------

-- Plain lists
instance List [] where
    showsList = showsSimpleList
    printList = printSimpleList
    putList   = putSimpleList
    parseList = sexprList

newtype Canonical a = Canonical {fromCanonical :: [a]}
    deriving (Functor)
{-# RULES
"Canonical"           Canonical = unsafeCoerce
"fromCanonical"       fromCanonical = unsafeCoerce
  #-}
instance List Canonical where
    showsList = showsCanonicalList . fromCanonical
    printList = printCanonicalList . fromCanonical
    putList   = putCanonicalList . fromCanonical
    parseList sexp = Canonical <$> sexprList sexp
