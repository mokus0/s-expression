{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Main where

import Text.SExpr
import Text.SExpr.Parse
import Text.SExpr.Print
import Text.SExpr.Type (SExpr(..), Hinted(..))
import Test.QuickCheck
import Data.Monoid()
import Text.Show.Functions()
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Data.Char
import Data.Binary.Put
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Text.PrettyPrint (render)

import Text.Printf
import Control.Applicative
import Control.Monad
import System.Environment
import System.Random
import System.IO
import Data.List

instance (Arbitrary a, Arbitrary (l (SExpr l a))) => Arbitrary (SExpr l a) where
    arbitrary = oneof [Atom <$> arbitrary,
                       List <$> sized arbList]
        where 
            arbList sz  = resize (sz `div` 2) arbitrary
              
    coarbitrary (Atom a) = variant 0 . coarbitrary a
    coarbitrary (List l) = variant 1 . coarbitrary l

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


prop_atoms :: Int -> Bool
prop_atoms n = Just n == (fromAtom $ atom n)

prop_foldMap :: (Int -> [Int]) -> SExpr [] Int -> Bool
prop_foldMap f s = T.foldMapDefault f s == F.foldMap f s

prop_fmap :: (Int -> Int) -> SExpr [] Int -> Bool
prop_fmap f s = T.fmapDefault f s == fmap f s

-- prop_readshow :: SExpr [] Int -> Bool
-- prop_readshow s = (read . show $ s) == s
-- 
-- prop_readshowStr :: SExpr [] String -> Bool
-- prop_readshowStr s = (readSExprString $ advancedString s) == s

prop_canonical_out :: SExpr [] String -> Bool
prop_canonical_out s = (readSExprString $ canonicalString s) == s

prop_canonical_in :: SExpr [] String -> Bool
prop_canonical_in s = (readCanonicalSExprString $ canonicalString s) == s

prop_put_canonical :: SExpr [] String -> Bool
prop_put_canonical s = 
    (L.unpack . runPut $ putCanonical s) == canonicalString s

prop_put_canonicalBS :: SExpr [] B.ByteString -> Bool
prop_put_canonicalBS s = 
    (L.unpack . runPut $ putCanonical s) == (canonicalString $ fmap B.unpack s)

prop_atom_raw :: String -> Bool
prop_atom_raw s = (readSExprString $ Text.SExpr.Print.raw s "") == atom s

prop_atom_token :: String -> Property
prop_atom_token s = canToken s ==> ((readSExprString s) == atom s)

prop_atom_hex :: String -> Property
prop_atom_hex s = canHex s ==> ((readSExprString $ render $ hex s) == atom s)

prop_atom_quote :: String -> Property
prop_atom_quote s = canQuote s ==> ((readSExprString $ render $ quote s) == atom s)

prop_atom_base64 :: String -> Bool
prop_atom_base64 s = (readSExprString $ render $ base64 s) == atom s 

instance Arbitrary B.ByteString where
    arbitrary = B.pack `fmap` arbitrary
    coarbitrary = undefined

instance Arbitrary Char where
  arbitrary     = choose (32,255) >>= \n -> return (chr n)
  coarbitrary n = variant (ord n)


main :: IO ()
main = do
  args <- fmap (drop 1) getArgs
  let n = if null args then 100 else read (head args)
  (results, passed) <- 
       liftM unzip $ mapM (\(s,a) -> printf "%-40s: " s >> a n) tests
  printf "Passed %d tests!\n" (sum passed)
  when (not . and $ results) $ fail "Not all tests passed!"

tests :: [(String, Int -> IO (Bool, Int))]
tests = [("Atom dis/assembly", mytest prop_atoms)
        ,("foldMap behaves as default", mytest prop_foldMap)
        ,("fmap behaves as default", mytest prop_fmap)
--         ,("(read.show)==id | Int", mytest prop_readshow)
--         ,("(read.show)==id | String", mytest prop_readshowStr)
        ,("canonical output", mytest prop_canonical_out)
        ,("canonical input", mytest prop_canonical_in)
        ,("efficient bytestring canonical", mytest prop_put_canonical)
        ,("efficient bytestring canonicalBS", mytest prop_put_canonicalBS)
        ,("token atom", mytest prop_atom_token)
        ,("hex atom", mytest prop_atom_hex)
        ,("quoted atom", mytest prop_atom_quote)
        ,("raw atom", mytest prop_atom_raw)
        ,("base64 atom", mytest prop_atom_base64)
        ]

------------------------------------------------------------------------
--
-- QC driver
-- copied from xmonad 0.8.1

debug :: Bool
debug = False

mytest :: Testable a => a -> Int -> IO (Bool, Int)
mytest a n = mycheck defaultConfig
    { configMaxTest=n
    , configEvery   = \nu _ -> let s = show nu in s ++ [ '\b' | _ <- s ] } a
 -- , configEvery= \n args -> if debug then show n ++ ":\n" ++ unlines args else [] } a

mycheck :: Testable a => Config -> a -> IO (Bool, Int)
mycheck config a = do
    rnd <- newStdGen
    mytests config (evaluate a) rnd 0 0 []

mytests :: Config -> Gen Result -> StdGen -> Int -> Int -> [[String]] -> IO (Bool, Int)
mytests config gen rnd0 ntest nfail stamps
    | ntest == configMaxTest config = done "OK," ntest stamps >> return (True, ntest)
    | nfail == configMaxFail config = done "Arguments exhausted after" ntest stamps >> return (True, ntest)
    | otherwise               =
      do putStr (configEvery config ntest (arguments result)) >> hFlush stdout
         case ok result of
           Nothing    ->
             mytests config gen rnd1 ntest (nfail+1) stamps
           Just True  ->
             mytests config gen rnd1 (ntest+1) nfail (stamp result:stamps)
           Just False ->
             putStr ( "Falsifiable after "
                   ++ show ntest
                   ++ " tests:\n"
                   ++ unlines (arguments result)
                    ) >> hFlush stdout >> return (False, ntest)
     where
      result      = generate (configSize config ntest) rnd2 gen
      (rnd1,rnd2) = split rnd0

done :: String -> Int -> [[String]] -> IO ()
done mesg ntest stamps = putStr ( mesg ++ " " ++ show ntest ++ " tests" ++ table )
  where
    table = display
            . map entry
            . reverse
            . sort
            . map pairLength
            . group
            . sort
            . filter (not . null)
            $ stamps

    display []  = ".\n"
    display [x] = " (" ++ x ++ ").\n"
    display xs  = ".\n" ++ unlines (map (++ ".") xs)

    pairLength xss@(xs:_) = (length xss, xs)
    pairLength [] = (0,undefined)
    entry (n, xs)         = percentage n ntest
                       ++ " "
                       ++ concat (intersperse ", " xs)

    percentage n m        = show ((100 * n) `div` m) ++ "%"

------------------------------------------------------------------------


