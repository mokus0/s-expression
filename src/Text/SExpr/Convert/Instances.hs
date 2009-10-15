{-# LANGUAGE 
        FlexibleInstances, FlexibleContexts
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


-- Hinted atoms
instance (Atom h, Atom s) => Atom (Hinted h s) where
    showsAtom = showsHinted
    printAtom = printHinted
    putAtom   = putHinted
    parseAtom = hintedAtom

-- |Raw strings for the canonical and basic encodings
newtype Raw s = Raw { fromRaw :: s }
instance Atom (Raw String) where
    showsAtom (Raw s) = raw s
    printAtom (Raw s) = rawDoc s
    putAtom   (Raw s) = putRaw s
    parseAtom = Raw <$> rawString

instance Atom (Raw B.ByteString) where
    showsAtom (Raw s) = rawBS s
    printAtom (Raw s) = rawDocBS s
    putAtom   (Raw s) = putRawBS s
    parseAtom = Raw . B.pack <$> rawString

instance Atom (Raw BL.ByteString) where
    showsAtom (Raw s) = rawBSL s
    printAtom (Raw s) = rawDocBSL s
    putAtom   (Raw s) = putRawBSL s
    parseAtom = Raw . BL.pack <$> rawString

instance (Atom (Raw h), Atom (Raw s)) => Atom (Raw (Hinted h s)) where
    showsAtom (Raw hs) = showsAtom (mapHint Raw Raw hs)
    printAtom (Raw hs) = printAtom (mapHint Raw Raw hs)
    putAtom   (Raw hs) = putAtom   (mapHint Raw Raw hs)
    parseAtom = (Raw . mapHint fromRaw fromRaw) <$> parseAtom

-- | \<simple-string\>s in the \"advanced\" encoding.  No formatting information is
-- retained.
data Simple s = Simple { fromSimple :: s }
instance Atom (Simple String) where
    printAtom (Simple s) = format s
    parseAtom = Simple <$> simpleString
    
instance Atom (Simple B.ByteString) where
    printAtom (Simple s) = format (B.unpack s)
    parseAtom = (Simple . B.pack) <$> simpleString
    
instance Atom (Simple BL.ByteString) where
    printAtom (Simple s) = format (BL.unpack s)
    parseAtom = (Simple . BL.pack) <$> simpleString
    
instance (Atom (Simple h), Atom (Simple s)) => Atom (Simple (Hinted h s)) where
    showsAtom (Simple hs) = showsAtom (mapHint Simple Simple hs)
    printAtom (Simple hs) = printAtom (mapHint Simple Simple hs)
    putAtom   (Simple hs) = putAtom   (mapHint Simple Simple hs)
    parseAtom = (Simple . mapHint fromSimple fromSimple) <$> parseAtom


--------------------------
-- List implementations --
--------------------------

-- Plain lists
instance List [] where
    showsList = showParen True . unwords'
        where
            unwords' [] = id
            unwords' [x] = x
            unwords' (x:xs) = x . showChar ' ' . unwords' xs
    printList = parens . sep
    putList l = do
        put '('
        sequence_ (intersperse (put ' ') l)
        put ')'
    parseList = sexprList

