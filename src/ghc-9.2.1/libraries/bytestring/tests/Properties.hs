{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
--
-- Must have rules off, otherwise the rewrite rules will replace the rhs
-- with the lhs, and we only end up testing lhs == lhs
--

--
-- -fhpc interferes with rewrite rules firing.
--

import Foreign.C.String (withCString)
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import GHC.Ptr
import Test.Tasty.QuickCheck
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Exception
import System.Posix.Internals (c_unlink)

import Data.List
import Data.Char
import Data.Word
import Data.Maybe
import Data.Int (Int64)
import Data.Monoid
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup
#endif
import GHC.Exts (Int(..), newPinnedByteArray#, unsafeFreezeByteArray#)
import GHC.ST (ST(..), runST)

import Text.Printf
import Data.String

import System.Environment
import System.IO

import Data.ByteString.Lazy (ByteString(..), pack , unpack)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Internal (ByteString(..))

import qualified Data.ByteString            as P
import qualified Data.ByteString.Internal   as P
import qualified Data.ByteString.Unsafe     as P
import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Short      as Short

import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Lazy.Char8 as D

import qualified Data.ByteString.Lazy.Internal as L
import Prelude hiding (abs)

import Rules
import QuickCheckUtils
import Test.Tasty
import Test.Tasty.QuickCheck

toInt64 :: Int -> Int64
toInt64 = fromIntegral

--
-- ByteString.Lazy.Char8 <=> ByteString.Char8
--

prop_concatCC       = D.concat                `eq1`  C.concat
prop_nullCC         = D.null                  `eq1`  C.null
prop_reverseCC      = D.reverse               `eq1`  C.reverse
prop_transposeCC    = D.transpose             `eq1`  C.transpose
prop_groupCC        = D.group                 `eq1`  C.group
prop_groupByCC      = D.groupBy               `eq2`  C.groupBy
prop_initsCC        = D.inits                 `eq1`  C.inits
prop_tailsCC        = D.tails                 `eq1`  C.tails
prop_allCC          = D.all                   `eq2`  C.all
prop_anyCC          = D.any                   `eq2`  C.any
prop_appendCC       = D.append                `eq2`  C.append
prop_breakCC        = D.break                 `eq2`  C.break
prop_concatMapCC    = forAll (sized $ \n -> resize (min 50 n) arbitrary) $
                      D.concatMap             `eq2`  C.concatMap
prop_consCC         = D.cons                  `eq2`  C.cons
prop_consCC'        = D.cons'                 `eq2`  C.cons
prop_unconsCC       = D.uncons                `eq1`  C.uncons
prop_unsnocCC       = D.unsnoc                `eq1`  C.unsnoc
prop_countCC        = D.count                 `eq2`  ((toInt64 .) . C.count)
prop_dropCC         = (D.drop . toInt64)      `eq2`  C.drop
prop_dropWhileCC    = D.dropWhile             `eq2`  C.dropWhile
prop_filterCC       = D.filter                `eq2`  C.filter
prop_findCC         = D.find                  `eq2`  C.find
prop_findIndexCC    = D.findIndex             `eq2`  ((fmap toInt64 .) . C.findIndex)
prop_findIndexEndCC = D.findIndexEnd          `eq2`  ((fmap toInt64 .) . C.findIndexEnd)
prop_findIndicesCC  = D.findIndices           `eq2`  ((fmap toInt64 .) . C.findIndices)
prop_isPrefixOfCC   = D.isPrefixOf            `eq2`  C.isPrefixOf
prop_stripPrefixCC  = D.stripPrefix           `eq2`  C.stripPrefix
prop_isSuffixOfCC   = D.isSuffixOf            `eq2`  C.isSuffixOf
prop_stripSuffixCC  = D.stripSuffix           `eq2`  C.stripSuffix
prop_mapCC          = D.map                   `eq2`  C.map
prop_replicateCC    = forAll arbitrarySizedIntegral $
                      (D.replicate . toInt64) `eq2`  C.replicate
prop_snocCC         = D.snoc                  `eq2`  C.snoc
prop_spanCC         = D.span                  `eq2`  C.span
prop_splitCC        = D.split                 `eq2`  C.split
prop_splitAtCC      = (D.splitAt . toInt64)   `eq2`  C.splitAt
prop_takeCC         = (D.take    . toInt64)   `eq2`  C.take
prop_takeWhileCC    = D.takeWhile             `eq2`  C.takeWhile
prop_elemCC         = D.elem                  `eq2`  C.elem
prop_notElemCC      = D.notElem               `eq2`  C.notElem
prop_elemIndexCC    = D.elemIndex             `eq2`  ((fmap toInt64 .) . C.elemIndex)
prop_elemIndicesCC  = D.elemIndices           `eq2`  ((fmap toInt64 .) . C.elemIndices)
prop_lengthCC       = D.length                `eq1`  (toInt64 . C.length)

prop_headCC         = D.head        `eqnotnull1` C.head
prop_initCC         = D.init        `eqnotnull1` C.init
prop_lastCC         = D.last        `eqnotnull1` C.last
prop_maximumCC      = D.maximum     `eqnotnull1` C.maximum
prop_minimumCC      = D.minimum     `eqnotnull1` C.minimum
prop_tailCC         = D.tail        `eqnotnull1` C.tail
prop_foldl1CC       = D.foldl1      `eqnotnull2` C.foldl1
prop_foldl1CC'      = D.foldl1'     `eqnotnull2` C.foldl1'
prop_foldr1CC       = D.foldr1      `eqnotnull2` C.foldr1
prop_foldr1CC'      = D.foldr1      `eqnotnull2` C.foldr1'
prop_scanlCC        = D.scanl       `eqnotnull3` C.scanl

prop_intersperseCC = D.intersperse  `eq2` C.intersperse

prop_foldlCC     = eq3
    (D.foldl     :: (X -> Char -> X) -> X -> B -> X)
    (C.foldl     :: (X -> Char -> X) -> X -> P -> X)
prop_foldlCC'    = eq3
    (D.foldl'    :: (X -> Char -> X) -> X -> B -> X)
    (C.foldl'    :: (X -> Char -> X) -> X -> P -> X)
prop_foldrCC     = eq3
    (D.foldr     :: (Char -> X -> X) -> X -> B -> X)
    (C.foldr     :: (Char -> X -> X) -> X -> P -> X)
prop_foldrCC'    = eq3
    (D.foldr     :: (Char -> X -> X) -> X -> B -> X)
    (C.foldr'    :: (Char -> X -> X) -> X -> P -> X)
prop_mapAccumLCC = eq3
    (D.mapAccumL :: (X -> Char -> (X,Char)) -> X -> B -> (X, B))
    (C.mapAccumL :: (X -> Char -> (X,Char)) -> X -> P -> (X, P))

--prop_mapIndexedCC = D.mapIndexed `eq2` C.mapIndexed
--prop_mapIndexedPL = L.mapIndexed `eq2` P.mapIndexed

--prop_mapAccumL_mapIndexedBP =
--        P.mapIndexed `eq2`
--        (\k p -> snd $ P.mapAccumL (\i w -> (i+1, k i w)) (0::Int) p)

--
-- ByteString.Lazy <=> ByteString
--

prop_concatBP       = forAll (sized $ \n -> resize (n `div` 2) arbitrary) $
                      L.concat               `eq1`  P.concat
prop_nullBP         = L.null                 `eq1`  P.null
prop_reverseBP      = L.reverse              `eq1`  P.reverse

prop_transposeBP    = L.transpose            `eq1`  P.transpose
prop_groupBP        = L.group                `eq1`  P.group
prop_groupByBP      = L.groupBy              `eq2`  P.groupBy
prop_initsBP        = L.inits                `eq1`  P.inits
prop_tailsBP        = L.tails                `eq1`  P.tails
prop_allBP          = L.all                  `eq2`  P.all
prop_anyBP          = L.any                  `eq2`  P.any
prop_appendBP       = L.append               `eq2`  P.append
prop_breakBP        = L.break                `eq2`  P.break
prop_concatMapBP    = forAll (sized $ \n -> resize (n `div` 4) arbitrary) $
                      L.concatMap            `eq2`  P.concatMap
prop_consBP         = L.cons                 `eq2`  P.cons
prop_consBP'        = L.cons'                `eq2`  P.cons
prop_unconsBP       = L.uncons               `eq1`  P.uncons
prop_unsnocBP       = L.unsnoc               `eq1`  P.unsnoc
prop_countBP        = L.count                `eq2`  ((toInt64 .) . P.count)
prop_dropBP         = (L.drop. toInt64)      `eq2`  P.drop
prop_dropWhileBP    = L.dropWhile            `eq2`  P.dropWhile
prop_filterBP       = L.filter               `eq2`  P.filter
prop_findBP         = L.find                 `eq2`  P.find
prop_findIndexBP    = L.findIndex            `eq2`  ((fmap toInt64 .) . P.findIndex)
prop_findIndexEndBP = L.findIndexEnd         `eq2`  ((fmap toInt64 .) . P.findIndexEnd)
prop_findIndicesBP  = L.findIndices          `eq2`  ((fmap toInt64 .) . P.findIndices)
prop_isPrefixOfBP   = L.isPrefixOf           `eq2`  P.isPrefixOf
prop_stripPrefixBP  = L.stripPrefix          `eq2`  P.stripPrefix
prop_isSuffixOfBP   = L.isSuffixOf           `eq2`  P.isSuffixOf
prop_stripSuffixBP  = L.stripSuffix          `eq2`  P.stripSuffix
prop_mapBP          = L.map                  `eq2`  P.map
prop_replicateBP    = forAll arbitrarySizedIntegral $
                      (L.replicate. toInt64) `eq2`  P.replicate
prop_snocBP         = L.snoc                 `eq2`  P.snoc
prop_spanBP         = L.span                 `eq2`  P.span
prop_splitBP        = L.split                `eq2`  P.split
prop_splitAtBP      = (L.splitAt. toInt64)   `eq2`  P.splitAt
prop_takeBP         = (L.take   . toInt64)   `eq2`  P.take
prop_takeWhileBP    = L.takeWhile            `eq2`  P.takeWhile
prop_elemBP         = L.elem                 `eq2`  P.elem
prop_notElemBP      = L.notElem              `eq2`  P.notElem
prop_elemIndexBP    = L.elemIndex            `eq2`  ((fmap toInt64 .) . P.elemIndex)
prop_elemIndexEndBP = L.elemIndexEnd         `eq2`  ((fmap toInt64 .) . P.elemIndexEnd)
prop_elemIndicesBP  = L.elemIndices          `eq2`  ((fmap toInt64 .) . P.elemIndices)
prop_intersperseBP  = L.intersperse          `eq2`  P.intersperse
prop_lengthBP       = L.length               `eq1`  (toInt64 . P.length)
prop_readIntBP      = D.readInt              `eq1`  C.readInt
prop_linesBP        = D.lines                `eq1`  C.lines

-- double check:
-- Currently there's a bug in the lazy bytestring version of lines, this
-- catches it:
prop_linesNLBP      = eq1 D.lines C.lines x
    where x = D.pack "one\ntwo\n\n\nfive\n\nseven\n"

prop_headBP         = L.head        `eqnotnull1` P.head
prop_initBP         = L.init        `eqnotnull1` P.init
prop_lastBP         = L.last        `eqnotnull1` P.last
prop_maximumBP      = L.maximum     `eqnotnull1` P.maximum
prop_minimumBP      = L.minimum     `eqnotnull1` P.minimum
prop_tailBP         = L.tail        `eqnotnull1` P.tail
prop_foldl1BP       = L.foldl1      `eqnotnull2` P.foldl1
prop_foldl1BP'      = L.foldl1'     `eqnotnull2` P.foldl1'
prop_foldr1BP       = L.foldr1      `eqnotnull2` P.foldr1
prop_foldr1BP'      = L.foldr1      `eqnotnull2` P.foldr1'
prop_scanlBP        = L.scanl       `eqnotnull3` P.scanl


prop_eqBP        = eq2
    ((==) :: B -> B -> Bool)
    ((==) :: P -> P -> Bool)
prop_compareBP   = eq2
    ((compare) :: B -> B -> Ordering)
    ((compare) :: P -> P -> Ordering)
prop_foldlBP     = eq3
    (L.foldl     :: (X -> W -> X) -> X -> B -> X)
    (P.foldl     :: (X -> W -> X) -> X -> P -> X)
prop_foldlBP'    = eq3
    (L.foldl'    :: (X -> W -> X) -> X -> B -> X)
    (P.foldl'    :: (X -> W -> X) -> X -> P -> X)
prop_foldrBP     = eq3
    (L.foldr     :: (W -> X -> X) -> X -> B -> X)
    (P.foldr     :: (W -> X -> X) -> X -> P -> X)
prop_foldrBP'    = eq3
    (L.foldr     :: (W -> X -> X) -> X -> B -> X)
    (P.foldr'    :: (W -> X -> X) -> X -> P -> X)
prop_mapAccumLBP = eq3
    (L.mapAccumL :: (X -> W -> (X,W)) -> X -> B -> (X, B))
    (P.mapAccumL :: (X -> W -> (X,W)) -> X -> P -> (X, P))

prop_unfoldrBP   =
  forAll arbitrarySizedIntegral $
  eq3
    ((\n f a -> L.take (fromIntegral n) $
        L.unfoldr    f a) :: Int -> (X -> Maybe (W,X)) -> X -> B)
    ((\n f a ->                     fst $
        P.unfoldrN n f a) :: Int -> (X -> Maybe (W,X)) -> X -> P)

prop_unfoldr2BP   =
  forAll arbitrarySizedIntegral $ \n ->
  forAll arbitrarySizedIntegral $ \a ->
  eq2
    ((\n a -> P.take (n*100) $
        P.unfoldr    (\x -> if x <= (n*100) then Just (fromIntegral x, x + 1) else Nothing) a)
                :: Int -> Int -> P)
    ((\n a ->                     fst $
        P.unfoldrN (n*100) (\x -> if x <= (n*100) then Just (fromIntegral x, x + 1) else Nothing) a)
                :: Int -> Int -> P)
    n a

prop_unfoldr2CP   =
  forAll arbitrarySizedIntegral $ \n ->
  forAll arbitrarySizedIntegral $ \a ->
  eq2
    ((\n a -> C.take (n*100) $
        C.unfoldr    (\x -> if x <= (n*100) then Just (chr (x `mod` 256), x + 1) else Nothing) a)
                :: Int -> Int -> P)
    ((\n a ->                     fst $
        C.unfoldrN (n*100) (\x -> if x <= (n*100) then Just (chr (x `mod` 256), x + 1) else Nothing) a)
                :: Int -> Int -> P)
    n a


prop_unfoldrLC   =
  forAll arbitrarySizedIntegral $
  eq3
    ((\n f a -> LC.take (fromIntegral n) $
        LC.unfoldr    f a) :: Int -> (X -> Maybe (Char,X)) -> X -> B)
    ((\n f a ->                     fst $
        C.unfoldrN n f a) :: Int -> (X -> Maybe (Char,X)) -> X -> P)

prop_cycleLC  a   =
  not (LC.null a) ==>
  forAll arbitrarySizedIntegral $
  eq1
    ((\n   -> LC.take (fromIntegral n) $
              LC.cycle a
     ) :: Int -> B)

    ((\n   -> LC.take (fromIntegral (n::Int)) . LC.concat $
              unfoldr (\x ->  Just (x,x) ) a
     ) :: Int -> B)


prop_iterateLC :: Int -> (Char8 -> Char8) -> Char8 -> Bool
prop_iterateLC n f (Char8 c) =
  eq3
    (\n f a -> LC.take (fromIntegral n) $ LC.iterate  f a)
    (\n f a -> fst $ C.unfoldrN n (\a -> Just (f a, f a)) a)
    n
    (castFn f :: Char -> Char)
    c

prop_iterateLC_2 :: Int -> (Char8 -> Char8) -> Char8 -> Bool
prop_iterateLC_2 n f (Char8 c) =
  eq3
    (\n f a -> LC.take (fromIntegral (n :: Int)) $ LC.iterate f a)
    (\n f a -> LC.take (fromIntegral (n :: Int)) $ LC.unfoldr (\a -> Just (f a, f a)) a)
    n
    (castFn f :: Char -> Char)
    c

prop_iterateL   =
  forAll arbitrarySizedIntegral $
  eq3
    ((\n f a -> L.take (fromIntegral n) $
        L.iterate  f a) :: Int -> (W -> W) -> W -> B)
    ((\n f a -> fst $
        P.unfoldrN n (\a -> Just (f a, f a)) a) :: Int -> (W -> W) -> W -> P)

prop_repeatLC   =
  forAll arbitrarySizedIntegral $
  eq2
    ((\n a -> LC.take (fromIntegral n) $
        LC.repeat a) :: Int -> Char -> B)
    ((\n a -> fst $
        C.unfoldrN n (\a -> Just (a, a)) a) :: Int -> Char -> P)

prop_repeatL   =
  forAll arbitrarySizedIntegral $
  eq2
    ((\n a -> L.take (fromIntegral n) $
        L.repeat a) :: Int -> W -> B)
    ((\n a -> fst $
        P.unfoldrN n (\a -> Just (a, a)) a) :: Int -> W -> P)

--
-- properties comparing ByteString.Lazy `eq1` List
--

prop_concatBL       = forAll (sized $ \n -> resize (n `div` 2) arbitrary) $
                      L.concat                `eq1` (concat    :: [[W]] -> [W])
prop_lengthBL       = L.length                `eq1` (toInt64 . length    :: [W] -> Int64)
prop_nullBL         = L.null                  `eq1` (null      :: [W] -> Bool)
prop_reverseBL      = L.reverse               `eq1` (reverse   :: [W] -> [W])
prop_transposeBL    = L.transpose             `eq1` (transpose :: [[W]] -> [[W]])
prop_groupBL        = L.group                 `eq1` (group     :: [W] -> [[W]])
prop_groupByBL      = L.groupBy               `eq2` (groupBy   :: (W -> W -> Bool) -> [W] -> [[W]])
prop_initsBL        = L.inits                 `eq1` (inits     :: [W] -> [[W]])
prop_tailsBL        = L.tails                 `eq1` (tails     :: [W] -> [[W]])
prop_allBL          = L.all                   `eq2` (all       :: (W -> Bool) -> [W] -> Bool)
prop_anyBL          = L.any                   `eq2` (any       :: (W -> Bool) -> [W] -> Bool)
prop_appendBL       = L.append                `eq2` ((++)      :: [W] -> [W] -> [W])
prop_breakBL        = L.break                 `eq2` (break     :: (W -> Bool) -> [W] -> ([W],[W]))
prop_concatMapBL    = forAll (sized $ \n -> resize (n `div` 2) arbitrary) $
                      L.concatMap             `eq2` (concatMap :: (W -> [W]) -> [W] -> [W])
prop_consBL         = L.cons                  `eq2` ((:)       :: W -> [W] -> [W])
prop_dropBL         = (L.drop . toInt64)      `eq2` (drop      :: Int -> [W] -> [W])
prop_dropWhileBL    = L.dropWhile             `eq2` (dropWhile :: (W -> Bool) -> [W] -> [W])
prop_filterBL       = L.filter                `eq2` (filter    :: (W -> Bool ) -> [W] -> [W])
prop_findBL         = L.find                  `eq2` (find      :: (W -> Bool) -> [W] -> Maybe W)
prop_findIndicesBL  = L.findIndices           `eq2` ((fmap toInt64 .) . findIndices:: (W -> Bool) -> [W] -> [Int64])
prop_findIndexBL    = L.findIndex             `eq2` ((fmap toInt64 .) . findIndex :: (W -> Bool) -> [W] -> Maybe Int64)
prop_findIndexEndBL = L.findIndexEnd          `eq2` ((fmap toInt64 .) . findIndexEnd :: (W -> Bool) -> [W] -> Maybe Int64)
prop_isPrefixOfBL   = L.isPrefixOf            `eq2` (isPrefixOf:: [W] -> [W] -> Bool)
prop_stripPrefixBL  = L.stripPrefix           `eq2` (stripPrefix:: [W] -> [W] -> Maybe [W])
prop_isSuffixOfBL   = L.isSuffixOf            `eq2` (isSuffixOf:: [W] -> [W] -> Bool)
prop_stripSuffixBL  = L.stripSuffix           `eq2` (stripSuffix :: [W] -> [W] -> Maybe [W])
prop_mapBL          = L.map                   `eq2` (map       :: (W -> W) -> [W] -> [W])
prop_replicateBL    = forAll arbitrarySizedIntegral $
                      (L.replicate . toInt64) `eq2` (replicate :: Int -> W -> [W])
prop_snocBL         = L.snoc                  `eq2` ((\xs x -> xs ++ [x]) :: [W] -> W -> [W])
prop_spanBL         = L.span                  `eq2` (span      :: (W -> Bool) -> [W] -> ([W],[W]))
prop_splitAtBL      = (L.splitAt . toInt64)   `eq2` (splitAt :: Int -> [W] -> ([W],[W]))
prop_takeBL         = (L.take    . toInt64)   `eq2` (take    :: Int -> [W] -> [W])
prop_takeWhileBL    = L.takeWhile             `eq2` (takeWhile :: (W -> Bool) -> [W] -> [W])
prop_elemBL         = L.elem                  `eq2` (elem      :: W -> [W] -> Bool)
prop_notElemBL      = L.notElem               `eq2` (notElem   :: W -> [W] -> Bool)
prop_elemIndexBL    = L.elemIndex             `eq2` ((fmap toInt64 .) . elemIndex   :: W -> [W] -> Maybe Int64)
prop_elemIndexEndBL = L.elemIndexEnd          `eq2` ((fmap toInt64 .) . elemIndexEnd:: W -> [W] -> Maybe Int64)
prop_elemIndicesBL  = L.elemIndices           `eq2` ((fmap toInt64 .) . elemIndices :: W -> [W] -> [Int64])
prop_linesBL        = D.lines                 `eq1` (lines     :: String -> [String])

prop_foldl1BL       = L.foldl1  `eqnotnull2` (foldl1    :: (W -> W -> W) -> [W] -> W)
prop_foldl1BL'      = L.foldl1' `eqnotnull2` (foldl1'   :: (W -> W -> W) -> [W] -> W)
prop_foldr1BL       = L.foldr1  `eqnotnull2` (foldr1    :: (W -> W -> W) -> [W] -> W)
prop_headBL         = L.head    `eqnotnull1` (head      :: [W] -> W)
prop_initBL         = L.init    `eqnotnull1` (init      :: [W] -> [W])
prop_lastBL         = L.last    `eqnotnull1` (last      :: [W] -> W)
prop_maximumBL      = L.maximum `eqnotnull1` (maximum   :: [W] -> W)
prop_minimumBL      = L.minimum `eqnotnull1` (minimum   :: [W] -> W)
prop_tailBL         = L.tail    `eqnotnull1` (tail      :: [W] -> [W])

prop_eqBL         = eq2
    ((==) :: B   -> B   -> Bool)
    ((==) :: [W] -> [W] -> Bool)
prop_compareBL    = eq2
    ((compare) :: B   -> B   -> Ordering)
    ((compare) :: [W] -> [W] -> Ordering)
prop_foldlBL      = eq3
    (L.foldl  :: (X -> W -> X) -> X -> B   -> X)
    (  foldl  :: (X -> W -> X) -> X -> [W] -> X)
prop_foldlBL'     = eq3
    (L.foldl' :: (X -> W -> X) -> X -> B   -> X)
    (  foldl' :: (X -> W -> X) -> X -> [W] -> X)
prop_foldrBL      = eq3
    (L.foldr  :: (W -> X -> X) -> X -> B   -> X)
    (  foldr  :: (W -> X -> X) -> X -> [W] -> X)
prop_mapAccumLBL  = eq3
    (L.mapAccumL :: (X -> W -> (X,W)) -> X -> B   -> (X, B))
    (  mapAccumL :: (X -> W -> (X,W)) -> X -> [W] -> (X, [W]))

prop_mapAccumRBL  = eq3
    (L.mapAccumR :: (X -> W -> (X,W)) -> X -> B   -> (X, B))
    (  mapAccumR :: (X -> W -> (X,W)) -> X -> [W] -> (X, [W]))

prop_mapAccumRDL :: (X -> Char8 -> (X, Char8)) -> X -> B -> Bool
prop_mapAccumRDL f = eq3
    (D.mapAccumR :: (X -> Char -> (X,Char)) -> X -> B   -> (X, B))
    (  mapAccumR :: (X -> Char -> (X,Char)) -> X -> [Char] -> (X, [Char]))
    (castFn f)

prop_mapAccumRCC :: (X -> Char8 -> (X, Char8)) -> X -> P -> Bool
prop_mapAccumRCC f = eq3
    (C.mapAccumR :: (X -> Char -> (X,Char)) -> X -> P   -> (X, P))
    (  mapAccumR :: (X -> Char -> (X,Char)) -> X -> [Char] -> (X, [Char]))
    (castFn f)

prop_unfoldrBL =
  forAll arbitrarySizedIntegral $
  eq3
    ((\n f a -> L.take (fromIntegral n) $
        L.unfoldr f a) :: Int -> (X -> Maybe (W,X)) -> X -> B)
    ((\n f a ->                  take n $
          unfoldr f a) :: Int -> (X -> Maybe (W,X)) -> X -> [W])

prop_packZipWithBL   = L.packZipWith `eq3` (zipWith :: (W -> W -> W) -> [W] -> [W] -> [W])

--
-- And finally, check correspondance between Data.ByteString and List
--

prop_lengthPL     = (fromIntegral.P.length :: P -> Int) `eq1` (length :: [W] -> Int)
prop_nullPL       = P.null      `eq1` (null      :: [W] -> Bool)
prop_reversePL    = P.reverse   `eq1` (reverse   :: [W] -> [W])
prop_transposePL  = P.transpose `eq1` (transpose :: [[W]] -> [[W]])
prop_groupPL      = P.group     `eq1` (group     :: [W] -> [[W]])
prop_groupByPL    = P.groupBy   `eq2` (groupBy   :: (W -> W -> Bool) -> [W] -> [[W]])
prop_initsPL      = P.inits     `eq1` (inits     :: [W] -> [[W]])
prop_tailsPL      = P.tails     `eq1` (tails     :: [W] -> [[W]])
prop_concatPL     = forAll (sized $ \n -> resize (n `div` 2) arbitrary) $
                    P.concat    `eq1` (concat    :: [[W]] -> [W])
prop_allPL        = P.all       `eq2` (all       :: (W -> Bool) -> [W] -> Bool)
prop_anyPL        = P.any       `eq2`    (any       :: (W -> Bool) -> [W] -> Bool)
prop_appendPL     = P.append    `eq2`    ((++)      :: [W] -> [W] -> [W])
prop_breakPL      = P.break     `eq2`    (break     :: (W -> Bool) -> [W] -> ([W],[W]))
prop_concatMapPL  = forAll (sized $ \n -> resize (n `div` 2) arbitrary) $
                    P.concatMap `eq2`    (concatMap :: (W -> [W]) -> [W] -> [W])
prop_consPL       = P.cons      `eq2`    ((:)       :: W -> [W] -> [W])
prop_dropPL       = P.drop      `eq2`    (drop      :: Int -> [W] -> [W])
prop_dropWhilePL  = P.dropWhile `eq2`    (dropWhile :: (W -> Bool) -> [W] -> [W])
prop_filterPL     = P.filter    `eq2`    (filter    :: (W -> Bool ) -> [W] -> [W])
prop_filterPL_rule= (\x -> P.filter ((==) x))  `eq2` -- test rules
                    ((\x -> filter ((==) x)) :: W -> [W] -> [W])

-- under lambda doesn't fire?
prop_filterLC_rule= (f)  `eq2` -- test rules
                    ((\x -> filter ((==) x)) :: Char -> [Char] -> [Char])
    where
         f x s = LC.filter ((==) x) s

prop_partitionPL  = P.partition `eq2`    (partition :: (W -> Bool ) -> [W] -> ([W],[W]))
prop_partitionLL  = L.partition `eq2`    (partition :: (W -> Bool ) -> [W] -> ([W],[W]))
prop_findPL       = P.find      `eq2`    (find      :: (W -> Bool) -> [W] -> Maybe W)
prop_findIndexPL  = P.findIndex `eq2`    (findIndex :: (W -> Bool) -> [W] -> Maybe Int)
prop_findIndexEndPL = P.findIndexEnd `eq2` (findIndexEnd :: (W -> Bool) -> [W] -> Maybe Int)
prop_isPrefixOfPL = P.isPrefixOf`eq2`    (isPrefixOf:: [W] -> [W] -> Bool)
prop_isSuffixOfPL = P.isSuffixOf`eq2`    (isSuffixOf:: [W] -> [W] -> Bool)
prop_isInfixOfPL  = P.isInfixOf `eq2`    (isInfixOf:: [W] -> [W] -> Bool)
prop_stripPrefixPL = P.stripPrefix`eq2`  (stripPrefix:: [W] -> [W] -> Maybe [W])
prop_stripSuffixPL = P.stripSuffix`eq2`  (stripSuffix:: [W] -> [W] -> Maybe [W])
prop_mapPL        = P.map       `eq2`    (map       :: (W -> W) -> [W] -> [W])
prop_replicatePL  = forAll arbitrarySizedIntegral $
                    P.replicate `eq2`    (replicate :: Int -> W -> [W])
prop_snocPL       = P.snoc      `eq2`    ((\xs x -> xs ++ [x]) :: [W] -> W -> [W])
prop_spanPL       = P.span      `eq2`    (span      :: (W -> Bool) -> [W] -> ([W],[W]))
prop_splitAtPL    = P.splitAt   `eq2`    (splitAt   :: Int -> [W] -> ([W],[W]))
prop_takePL       = P.take      `eq2`    (take      :: Int -> [W] -> [W])
prop_takeWhilePL  = P.takeWhile `eq2`    (takeWhile :: (W -> Bool) -> [W] -> [W])
prop_elemPL       = P.elem      `eq2`    (elem      :: W -> [W] -> Bool)
prop_notElemPL    = P.notElem   `eq2`    (notElem   :: W -> [W] -> Bool)
prop_elemIndexPL  = P.elemIndex `eq2`    (elemIndex :: W -> [W] -> Maybe Int)
prop_linesPL      = C.lines     `eq1`    (lines     :: String -> [String])
prop_findIndicesPL= P.findIndices`eq2`   (findIndices:: (W -> Bool) -> [W] -> [Int])
prop_elemIndicesPL= P.elemIndices`eq2`   (elemIndices:: W -> [W] -> [Int])
prop_zipPL        = P.zip        `eq2`   (zip :: [W] -> [W] -> [(W,W)])
prop_zipCL        = C.zip        `eq2`   (zip :: [Char] -> [Char] -> [(Char,Char)])
prop_zipLL        = L.zip        `eq2`   (zip :: [W] -> [W] -> [(W,W)])
prop_unzipPL      = P.unzip      `eq1`   (unzip :: [(W,W)] -> ([W],[W]))
prop_unzipLL      = L.unzip      `eq1`   (unzip :: [(W,W)] -> ([W],[W]))

prop_unzipCL :: [(Char8, Char8)] -> Bool
prop_unzipCL xs   = (C.unzip     `eq1`   (unzip :: [(Char,Char)] -> ([Char],[Char])))
                    [ (a,b) | (Char8 a, Char8 b) <- xs ]

prop_unzipDL :: [(Char8, Char8)] -> Bool
prop_unzipDL xs   = (D.unzip     `eq1`   (unzip :: [(Char,Char)] -> ([Char],[Char])))
                    [ (a,b) | (Char8 a, Char8 b) <- xs ]

prop_foldl1PL     = P.foldl1    `eqnotnull2` (foldl1   :: (W -> W -> W) -> [W] -> W)
prop_foldl1PL'    = P.foldl1'   `eqnotnull2` (foldl1' :: (W -> W -> W) -> [W] -> W)
prop_foldr1PL     = P.foldr1    `eqnotnull2` (foldr1 :: (W -> W -> W) -> [W] -> W)
prop_scanlPL      = P.scanl     `eqnotnull3` (scanl  :: (W -> W -> W) -> W -> [W] -> [W])
prop_scanl1PL     = P.scanl1    `eqnotnull2` (scanl1 :: (W -> W -> W) -> [W] -> [W])
prop_scanrPL      = P.scanr     `eqnotnull3` (scanr  :: (W -> W -> W) -> W -> [W] -> [W])
prop_scanr1PL     = P.scanr1    `eqnotnull2` (scanr1 :: (W -> W -> W) -> [W] -> [W])
prop_headPL       = P.head      `eqnotnull1` (head      :: [W] -> W)
prop_initPL       = P.init      `eqnotnull1` (init      :: [W] -> [W])
prop_lastPL       = P.last      `eqnotnull1` (last      :: [W] -> W)
prop_maximumPL    = P.maximum   `eqnotnull1` (maximum   :: [W] -> W)
prop_minimumPL    = P.minimum   `eqnotnull1` (minimum   :: [W] -> W)
prop_tailPL       = P.tail      `eqnotnull1` (tail      :: [W] -> [W])

prop_scanl1CL :: (Char8 -> Char8 -> Char8) -> P -> Property
prop_scanrCL  :: (Char8 -> Char8 -> Char8) -> Char8 -> P -> Property
prop_scanr1CL :: (Char8 -> Char8 -> Char8) -> P -> Property

prop_scanl1CL f = eqnotnull2
    C.scanl1
    (scanl1 :: (Char -> Char -> Char) -> [Char] -> [Char])
    (castFn f)

prop_scanrCL f (Char8 c) = eqnotnull3
    C.scanr
    (scanr  :: (Char -> Char -> Char) -> Char -> [Char] -> [Char])
    (castFn f)
    c

prop_scanr1CL f = eqnotnull2
    C.scanr1
    (scanr1 :: (Char -> Char -> Char) -> [Char] -> [Char])
    (castFn f)

prop_packZipWithPL   = P.packZipWith  `eq3` (zipWith :: (W -> W -> W) -> [W] -> [W] -> [W])

prop_zipWithPL    = (P.zipWith  :: (W -> W -> X) -> P   -> P   -> [X]) `eq3`
                      (zipWith  :: (W -> W -> X) -> [W] -> [W] -> [X])

prop_zipWithPL_rules   = (P.zipWith  :: (W -> W -> W) -> P -> P -> [W]) `eq3`
                         (zipWith    :: (W -> W -> W) -> [W] -> [W] -> [W])

prop_eqPL      = eq2
    ((==) :: P   -> P   -> Bool)
    ((==) :: [W] -> [W] -> Bool)
prop_comparePL = eq2
    ((compare) :: P   -> P   -> Ordering)
    ((compare) :: [W] -> [W] -> Ordering)
prop_foldlPL   = eq3
    (P.foldl  :: (X -> W -> X) -> X -> P        -> X)
    (  foldl  :: (X -> W -> X) -> X -> [W]      -> X)
prop_foldlPL'  = eq3
    (P.foldl' :: (X -> W -> X) -> X -> P        -> X)
    (  foldl' :: (X -> W -> X) -> X -> [W]      -> X)
prop_foldrPL   = eq3
    (P.foldr  :: (W -> X -> X) -> X -> P        -> X)
    (  foldr  :: (W -> X -> X) -> X -> [W]      -> X)
prop_mapAccumLPL= eq3
    (P.mapAccumL :: (X -> W -> (X,W)) -> X -> P -> (X, P))
    (  mapAccumL :: (X -> W -> (X,W)) -> X -> [W] -> (X, [W]))
prop_mapAccumRPL= eq3
    (P.mapAccumR :: (X -> W -> (X,W)) -> X -> P -> (X, P))
    (  mapAccumR :: (X -> W -> (X,W)) -> X -> [W] -> (X, [W]))
prop_unfoldrPL =
  forAll arbitrarySizedIntegral $
  eq3
    ((\n f a ->      fst $
        P.unfoldrN n f a) :: Int -> (X -> Maybe (W,X)) -> X -> P)
    ((\n f a ->   take n $
          unfoldr    f a) :: Int -> (X -> Maybe (W,X)) -> X -> [W])

------------------------------------------------------------------------
--
-- These are miscellaneous tests left over. Or else they test some
-- property internal to a type (i.e. head . sort == minimum), without
-- reference to a model type.
--

invariant :: L.ByteString -> Bool
invariant Empty       = True
invariant (Chunk c cs) = not (P.null c) && invariant cs

prop_invariant = invariant

prop_eq_refl  x     = x        == (x :: ByteString)
prop_eq_symm  x y   = (x == y) == (y == (x :: ByteString))

prop_eq1 xs      = xs == (unpack . pack $ xs)
prop_eq2 xs      = xs == (xs :: ByteString)
prop_eq3 xs ys   = (xs == ys) == (unpack xs == unpack ys)

prop_compare1 xs   = (pack xs        `compare` pack xs) == EQ
prop_compare2 xs c = (pack (xs++[c]) `compare` pack xs) == GT
prop_compare3 xs c = (pack xs `compare` pack (xs++[c])) == LT

prop_compare4 xs    = (not (null xs)) ==> (pack xs  `compare` L.empty) == GT
prop_compare5 xs    = (not (null xs)) ==> (L.empty `compare` pack xs) == LT
prop_compare6 xs ys = (not (null ys)) ==> (pack (xs++ys)  `compare` pack xs) == GT

prop_compare7 x  y  = x  `compare` y  == (L.singleton x `compare` L.singleton y)
prop_compare8 xs ys = xs `compare` ys == (L.pack xs `compare` L.pack ys)
prop_compare9       = (L.singleton 255 `compare` L.singleton 127) == GT

prop_compare7LL (Char8 x) (Char8 y) =
                      x  `compare` y  == (LC.singleton x `compare` LC.singleton y)

prop_empty1 = L.length L.empty == 0
prop_empty2 = L.unpack L.empty == []

prop_packunpack s = (L.unpack . L.pack) s == id s
prop_unpackpack s = (L.pack . L.unpack) s == id s

prop_null xs = null (L.unpack xs) == L.null xs

prop_length1 xs = fromIntegral (length xs) == L.length (L.pack xs)

prop_length2 xs = L.length xs == length1 xs
  where length1 ys
            | L.null ys = 0
            | otherwise = 1 + length1 (L.tail ys)

prop_cons1 c xs = unpack (L.cons c (pack xs)) == (c:xs)
prop_cons2 c    = L.singleton c == (c `L.cons` L.empty)
prop_cons3 c    = unpack (L.singleton c) == (c:[])
prop_cons4 c    = (c `L.cons` L.empty)  == pack (c:[])

prop_snoc1 xs c = xs ++ [c] == unpack ((pack xs) `L.snoc` c)

prop_head  xs = (not (null xs)) ==> head xs == (L.head . pack) xs
prop_head1 xs = not (L.null xs) ==> L.head xs == head (L.unpack xs)

prop_tail xs  = not (L.null xs) ==> L.tail xs == pack (tail (unpack xs))
prop_tail1 xs = (not (null xs)) ==> tail xs   == (unpack . L.tail . pack) xs

prop_last xs  = (not (null xs)) ==> last xs    == (L.last . pack) xs

prop_init xs  =
    (not (null xs)) ==>
    init xs   == (unpack . L.init . pack) xs

prop_append1 xs    = (xs ++ xs) == (unpack $ pack xs `L.append` pack xs)
prop_append2 xs ys = (xs ++ ys) == (unpack $ pack xs `L.append` pack ys)
prop_append3 xs ys = L.append xs ys == pack (unpack xs ++ unpack ys)
prop_appendLazy xs = L.head (L.pack [xs] `L.append` error "Tail should be lazy") == xs

prop_map1 f xs   = L.map f (pack xs)    == pack (map f xs)
prop_map2 f g xs = L.map f (L.map g xs) == L.map (f . g) xs
prop_map3 f xs   = map f xs == (unpack . L.map f .  pack) xs

prop_filter1 c xs = (filter (/=c) xs) == (unpack $ L.filter (/=c) (pack xs))
prop_filter2 p xs = (filter p xs) == (unpack $ L.filter p (pack xs))

prop_reverse  xs = reverse xs          == (unpack . L.reverse . pack) xs
prop_reverse1 xs = L.reverse (pack xs) == pack (reverse xs)
prop_reverse2 xs = reverse (unpack xs) == (unpack . L.reverse) xs

prop_transpose xs = (transpose xs) == ((map unpack) . L.transpose . (map pack)) xs

prop_foldl f c xs = L.foldl f c (pack xs) == foldl f c xs
    where _ = c :: Char

prop_foldr f c xs = L.foldl f c (pack xs) == foldl f c xs
    where _ = c :: Char

prop_foldl_1 xs = L.foldl (\xs c -> c `L.cons` xs) L.empty xs == L.reverse xs
prop_foldr_1 xs = L.foldr (\c xs -> c `L.cons` xs) L.empty xs == id xs

prop_foldl1_1 xs =
    (not . L.null) xs ==>
    L.foldl1 (\x c -> if c > x then c else x)   xs ==
    L.foldl  (\x c -> if c > x then c else x) 0 xs

prop_foldl1_2 xs =
    (not . L.null) xs ==>
    L.foldl1 const xs == L.head xs

prop_foldl1_3 xs =
    (not . L.null) xs ==>
    L.foldl1 (flip const) xs == L.last xs

prop_foldr1_1 xs =
    (not . L.null) xs ==>
    L.foldr1 (\c x -> if c > x then c else x)   xs ==
    L.foldr  (\c x -> if c > x then c else x) 0 xs

prop_foldr1_2 xs =
    (not . L.null) xs ==>
    L.foldr1 (flip const) xs == L.last xs

prop_foldr1_3 xs =
    (not . L.null) xs ==>
    L.foldr1 const xs == L.head xs

prop_concat1 xs = (concat [xs,xs]) == (unpack $ L.concat [pack xs, pack xs])
prop_concat2 xs = (concat [xs,[]]) == (unpack $ L.concat [pack xs, pack []])
prop_concat3    = forAll (sized $ \n -> resize (n `div` 2) arbitrary) $ \xss ->
                  L.concat (map pack xss) == pack (concat xss)

prop_concatMap xs = L.concatMap L.singleton xs == (pack . concatMap (:[]) . unpack) xs

prop_any xs a = (any (== a) xs) == (L.any (== a) (pack xs))
prop_all xs a = (all (== a) xs) == (L.all (== a) (pack xs))

prop_maximum xs = (not (null xs)) ==> (maximum xs) == (L.maximum ( pack xs ))
prop_minimum xs = (not (null xs)) ==> (minimum xs) == (L.minimum ( pack xs ))

prop_compareLength1 xs  =  (L.pack xs         `L.compareLength` fromIntegral (length xs)) == EQ
prop_compareLength2 xs c = (L.pack (xs ++ [c]) `L.compareLength` fromIntegral (length xs)) == GT
prop_compareLength3 xs c = (L.pack xs `L.compareLength` fromIntegral (length (xs ++ [c]))) == LT
prop_compareLength4 xs c = ((L.pack xs `L.append` L.pack [c] `L.append` L.pack [undefined])
                            `L.compareLength` fromIntegral (length xs)) == GT
prop_compareLength5 xs l = L.compareLength xs l == compare (L.length xs) l

prop_replicate1 c =
    forAll arbitrary $ \(Positive n) ->
    unpack (L.replicate (fromIntegral n) c) == replicate n c

prop_replicate2 c = unpack (L.replicate 0 c) == replicate 0 c

prop_take1 i xs = L.take (fromIntegral i) (pack xs) == pack (take i xs)
prop_takeEnd i xs = P.takeEnd i xs == P.drop (P.length xs - i) xs

prop_drop1 i xs = L.drop (fromIntegral i) (pack xs) == pack (drop i xs)
prop_dropEnd i xs = P.dropEnd i xs == P.take (P.length xs - i) xs

prop_splitAt i xs = --collect (i >= 0 && i < length xs) $
    L.splitAt (fromIntegral i) (pack xs) == let (a,b) = splitAt i xs in (pack a, pack b)

prop_takeWhile f xs = L.takeWhile f (pack xs) == pack (takeWhile f xs)
prop_dropWhile f xs = L.dropWhile f (pack xs) == pack (dropWhile f xs)
prop_takeWhileEnd f = P.takeWhileEnd f `eq1` (P.reverse . P.takeWhile f . P.reverse)
prop_dropWhileEnd f = P.dropWhileEnd f `eq1` (P.reverse . P.dropWhile f . P.reverse)

prop_break f xs = L.break f (pack xs) ==
    let (a,b) = break f xs in (pack a, pack b)

prop_breakspan xs c = L.break (==c) xs == L.span (/=c) xs

prop_span xs a = (span (/=a) xs) == (let (x,y) = L.span (/=a) (pack xs) in (unpack x, unpack y))

prop_split c xs = (map L.unpack . map checkInvariant . L.split c $ xs)
               == (map P.unpack . P.split c . P.pack . L.unpack $ xs)

prop_splitWith_empty f = L.splitWith f mempty == []

prop_splitWith f xs = (l1 == l2 || l1 == l2+1) &&
        sum (map L.length splits) == L.length xs - l2
  where splits = L.splitWith f xs
        l1 = fromIntegral (length splits)
        l2 = L.length (L.filter f xs)

prop_splitWith_D_empty f = D.splitWith f mempty == []

prop_splitWith_D f xs = (l1 == l2 || l1 == l2+1) &&
        sum (map D.length splits) == D.length xs - l2
  where splits = D.splitWith f xs
        l1 = fromIntegral (length splits)
        l2 = D.length (D.filter f xs)

prop_splitWith_C_empty f = C.splitWith f mempty == []

prop_splitWith_C f xs = (l1 == l2 || l1 == l2+1) &&
        sum (map C.length splits) == C.length xs - l2
  where splits = C.splitWith f xs
        l1 = fromIntegral (length splits)
        l2 = C.length (C.filter f xs)

prop_split_empty c = L.split c mempty == []

prop_joinsplit c xs = L.intercalate (pack [c]) (L.split c xs) == id xs

prop_group xs       = group xs == (map unpack . L.group . pack) xs
prop_groupBy  f xs  = groupBy f xs == (map unpack . L.groupBy f . pack) xs

prop_groupBy_LC :: (Char8 -> Char8 -> Bool) -> String8 -> Bool
prop_groupBy_LC f' (String8 xs) =
    groupBy f xs == (map LC.unpack . LC.groupBy f .  LC.pack) xs
  where
    f :: Char -> Char -> Bool
    f = castFn f'

-- prop_joinjoinByte xs ys c = L.joinWithByte c xs ys == L.join (L.singleton c) [xs,ys]

prop_index xs =
  not (null xs) ==>
    forAll indices $ \i -> (xs !! i) == L.pack xs `L.index` (fromIntegral i)
  where indices = choose (0, length xs -1)

prop_index_D (String8 xs) =
  not (null xs) ==>
    forAll indices $ \i -> (xs !! i) == D.pack xs `D.index` (fromIntegral i)
  where indices = choose (0, length xs -1)

prop_index_C (String8 xs) =
  not (null xs) ==>
    forAll indices $ \i -> (xs !! i) == C.pack xs `C.index` (fromIntegral i)
  where indices = choose (0, length xs -1)

-- | Test 'indexMaybe' for Lazy and Strict 'ByteString's.
--   If we are testing within the bounds it should return a 'Just' value.
--   If we are testing outside of the bounds it should return a 'Nothing' value.
prop_indexMaybe_Just_L xs =
  not (null xs) ==>
    forAll indices $ \i -> isJust (ys `L.indexMaybe` (fromIntegral i))
  where
    ys = L.pack xs
    indices = choose (0, length xs -1)

prop_indexMaybe_Just_P xs =
  not (null xs) ==>
    forAll indices $ \i -> isJust (ys `P.indexMaybe` (fromIntegral i))
  where
    ys = P.pack xs
    indices = choose (0, length xs -1)

prop_indexMaybe_Nothing_L xs =
  not (null xs) ==>
    forAll indices $ \i -> isNothing (ys `L.indexMaybe` (fromIntegral i))
  where
      ys = L.pack xs
      outOfBounds = choose (-100, length xs + 100)
      indices = suchThat outOfBounds (\n -> n < 0 || n >= length xs)

prop_indexMaybe_Nothing_P xs =
  not (null xs) ==>
    forAll indices $ \i -> isNothing (ys `P.indexMaybe` (fromIntegral i))
  where
    ys = P.pack xs
    outOfBounds = choose (-100, length xs + 100)
    indices = suchThat outOfBounds (\n -> n < 0 || n >= length xs)

prop_elemIndex xs c = (elemIndex c xs) == fmap fromIntegral (L.elemIndex c (pack xs))

prop_elemIndexCL :: String8 -> Char8 -> Bool
prop_elemIndexCL (String8 xs) (Char8 c) =
    (elemIndex c xs) == (C.elemIndex c (C.pack xs))

prop_elemIndices xs c = elemIndices c xs == map fromIntegral (L.elemIndices c (pack xs))

prop_count c xs = length (L.elemIndices c xs) == fromIntegral (L.count c xs)

prop_findIndex xs f = (findIndex f xs) == fmap fromIntegral (L.findIndex f (pack xs))
prop_findIndexEnd xs f = (findIndexEnd f xs) == fmap fromIntegral (L.findIndexEnd f (pack xs))
prop_findIndicies xs f = (findIndices f xs) == map fromIntegral (L.findIndices f (pack xs))

prop_elem    xs c = (c `elem` xs)    == (c `L.elem` (pack xs))
prop_notElem xs c = (c `notElem` xs) == (L.notElem c (pack xs))
prop_elem_notelem xs c = c `L.elem` xs == not (c `L.notElem` xs)

-- prop_filterByte  xs c = L.filterByte c xs == L.filter (==c) xs
-- prop_filterByte2 xs c = unpack (L.filterByte c xs) == filter (==c) (unpack xs)

-- prop_filterNotByte  xs c = L.filterNotByte c xs == L.filter (/=c) xs
-- prop_filterNotByte2 xs c = unpack (L.filterNotByte c xs) == filter (/=c) (unpack xs)

prop_find p xs = find p xs == L.find p (pack xs)

prop_find_findIndex p xs =
    L.find p xs == case L.findIndex p xs of
                                Just n -> Just (xs `L.index` n)
                                _      -> Nothing

prop_isPrefixOf xs ys = isPrefixOf xs ys == (pack xs `L.isPrefixOf` pack ys)
prop_stripPrefix xs ys = (pack <$> stripPrefix xs ys) == (pack xs `L.stripPrefix` pack ys)

prop_isSuffixOf xs ys = isSuffixOf xs ys == (pack xs `L.isSuffixOf` pack ys)
prop_stripSuffix xs ys = (pack <$> stripSuffix xs ys) == (pack xs `L.stripSuffix` pack ys)

{-
prop_sort1 xs = sort xs == (unpack . L.sort . pack) xs
prop_sort2 xs = (not (null xs)) ==> (L.head . L.sort . pack $ xs) == minimum xs
prop_sort3 xs = (not (null xs)) ==> (L.last . L.sort . pack $ xs) == maximum xs
prop_sort4 xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (L.head . L.sort) (L.append (pack xs) (pack ys)) == min (minimum xs) (minimum ys)

prop_sort5 xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (L.last . L.sort) (L.append (pack xs) (pack ys)) == max (maximum xs) (maximum ys)

-}

------------------------------------------------------------------------
-- Misc ByteString properties

prop_nil1BB = P.length P.empty == 0
prop_nil2BB = P.unpack P.empty == []
prop_nil1BB_monoid = P.length mempty == 0
prop_nil2BB_monoid = P.unpack mempty == []

prop_nil1LL_monoid = L.length mempty == 0
prop_nil2LL_monoid = L.unpack mempty == []

prop_tailSBB xs = not (P.null xs) ==> P.tail xs == P.pack (tail (P.unpack xs))

prop_nullBB xs = null (P.unpack xs) == P.null xs

prop_lengthBB xs = P.length xs == length1 xs
    where
        length1 ys
            | P.null ys = 0
            | otherwise = 1 + length1 (P.tail ys)

prop_lengthSBB xs = length xs == P.length (P.pack xs)

prop_indexBB xs =
  not (null xs) ==>
    forAll indices $ \i -> (xs !! i) == P.pack xs `P.index` i
  where indices = choose (0, length xs -1)

prop_unsafeIndexBB xs =
  not (null xs) ==>
    forAll indices $ \i -> (xs !! i) == P.pack xs `P.unsafeIndex` i
  where indices = choose (0, length xs -1)

prop_mapfusionBB f g xs = P.map f (P.map g xs) == P.map (f . g) xs

prop_filterBB f xs = P.filter f (P.pack xs) == P.pack (filter f xs)

prop_filterfusionBB f g xs = P.filter f (P.filter g xs) == P.filter (\c -> f c && g c) xs

prop_elemSBB x xs = P.elem x (P.pack xs) == elem x xs

prop_takeSBB i xs = P.take i (P.pack xs) == P.pack (take i xs)
prop_dropSBB i xs = P.drop i (P.pack xs) == P.pack (drop i xs)

prop_splitAtSBB i xs = -- collect (i >= 0 && i < length xs) $
    P.splitAt i (P.pack xs) ==
    let (a,b) = splitAt i xs in (P.pack a, P.pack b)

prop_foldlBB f c xs = P.foldl f c (P.pack xs) == foldl f c xs
  where _ = c :: Char

prop_scanlfoldlBB f z xs = not (P.null xs) ==> P.last (P.scanl f z xs) == P.foldl f z xs

prop_foldrBB f c xs = P.foldl f c (P.pack xs) == foldl f c xs
  where _ = c :: Char

prop_takeWhileSBB f xs = P.takeWhile f (P.pack xs) == P.pack (takeWhile f xs)
prop_dropWhileSBB f xs = P.dropWhile f (P.pack xs) == P.pack (dropWhile f xs)

prop_spanSBB f xs = P.span f (P.pack xs) ==
    let (a,b) = span f xs in (P.pack a, P.pack b)

prop_breakSBB f xs = P.break f (P.pack xs) ==
    let (a,b) = break f xs in (P.pack a, P.pack b)

prop_breakspan_1BB xs c = P.break (== c) xs == P.span (/= c) xs

prop_linesSBB (String8 xs) = C.lines (C.pack xs) == map C.pack (lines xs)

prop_unlinesSBB xss = C.unlines (map C.pack xss) == C.pack (unlines xss)

prop_wordsSBB (String8 xs) =
    C.words (C.pack xs) == map C.pack (words xs)

prop_wordsLC (String8 xs) =
    LC.words (LC.pack xs) == map LC.pack (words xs)

prop_unwordsSBB xss = C.unwords (map C.pack xss) == C.pack (unwords xss)
prop_unwordsSLC xss = LC.unwords (map LC.pack xss) == LC.pack (unwords xss)

prop_splitWithBB_empty f = P.splitWith f mempty == []

prop_splitWithBB f xs = (l1 == l2 || l1 == l2+1) &&
        sum (map P.length splits) == P.length xs - l2
  where splits = P.splitWith f xs
        l1 = length splits
        l2 = P.length (P.filter f xs)

prop_splitBB_empty c = P.split c mempty == []

prop_joinsplitBB c xs = P.intercalate (P.pack [c]) (P.split c xs) == xs

prop_intercalatePL c x y =

    P.intercalate (P.singleton c) (x : y : []) ==
 --     intercalate (singleton c) (s1 : s2 : [])

    P.pack (intercalate [c] [P.unpack x,P.unpack y])

-- prop_linessplitBB xs =
--     (not . C.null) xs ==>
--     C.lines' xs == C.split '\n' xs

-- false:
{-
prop_linessplit2BB xs =
   (not . C.null) xs ==>
    C.lines xs == C.split '\n' xs ++ (if C.last xs == '\n' then [C.empty] else [])
-}

prop_splitsplitWithBB c xs = P.split c xs == P.splitWith (== c) xs

prop_bijectionBB  (Char8 c) = (P.w2c . P.c2w) c == id c
prop_bijectionBB'        w  = (P.c2w . P.w2c) w == id w

prop_packunpackBB  s = (P.unpack . P.pack) s == id s
prop_packunpackBB' s = (P.pack . P.unpack) s == id s

prop_eq1BB xs      = xs            == (P.unpack . P.pack $ xs)
prop_eq2BB xs      = xs == (xs :: P.ByteString)
prop_eq3BB xs ys   = (xs == ys) == (P.unpack xs == P.unpack ys)

prop_compare1BB xs  = (P.pack xs         `compare` P.pack xs) == EQ
prop_compare2BB xs c = (P.pack (xs++[c]) `compare` P.pack xs) == GT
prop_compare3BB xs c = (P.pack xs `compare` P.pack (xs++[c])) == LT

prop_compare4BB xs  = (not (null xs)) ==> (P.pack xs  `compare` P.empty) == GT
prop_compare5BB xs  = (not (null xs)) ==> (P.empty `compare` P.pack xs) == LT
prop_compare6BB xs ys= (not (null ys)) ==> (P.pack (xs++ys)  `compare` P.pack xs) == GT

prop_compare7BB (Char8 x) (Char8 y) =
                        x  `compare` y  == (C.singleton x `compare` C.singleton y)
prop_compare8BB xs ys = xs `compare` ys == (P.pack xs `compare` P.pack ys)

prop_consBB  c xs = P.unpack (P.cons c (P.pack xs)) == (c:xs)
prop_cons1BB (String8 xs)
                  = 'X' : xs == C.unpack ('X' `C.cons` (C.pack xs))
prop_cons2BB xs c = c : xs == P.unpack (c `P.cons` (P.pack xs))
prop_cons3BB (Char8 c)
                  = C.unpack (C.singleton c) == (c:[])
prop_cons4BB c    = (c `P.cons` P.empty)  == P.pack (c:[])

prop_snoc1BB xs c = xs ++ [c] == P.unpack ((P.pack xs) `P.snoc` c)

prop_head1BB xs     = (not (null xs)) ==> head  xs  == (P.head . P.pack) xs
prop_head2BB xs    = (not (null xs)) ==> head xs   == (P.unsafeHead . P.pack) xs
prop_head3BB xs    = not (P.null xs) ==> P.head xs == head (P.unpack xs)

prop_tailBB xs     = (not (null xs)) ==> tail xs    == (P.unpack . P.tail . P.pack) xs
prop_tail1BB xs    = (not (null xs)) ==> tail xs    == (P.unpack . P.unsafeTail. P.pack) xs

prop_lastBB xs     = (not (null xs)) ==> last xs    == (P.last . P.pack) xs
prop_last1BB xs    = (not (null xs)) ==> last xs    == (P.unsafeLast . P.pack) xs

prop_initBB xs     =
    (not (null xs)) ==>
    init xs    == (P.unpack . P.init . P.pack) xs
prop_init1BB xs     =
    (not (null xs)) ==>
    init xs    == (P.unpack . P.unsafeInit . P.pack) xs

-- prop_null xs = (null xs) ==> null xs == (nullPS (pack xs))

prop_append1BB xs    = (xs ++ xs) == (P.unpack $ P.pack xs `P.append` P.pack xs)
prop_append2BB xs ys = (xs ++ ys) == (P.unpack $ P.pack xs `P.append` P.pack ys)
prop_append3BB xs ys = P.append xs ys == P.pack (P.unpack xs ++ P.unpack ys)

prop_append1BB_monoid xs    = (xs ++ xs) == (P.unpack $ P.pack xs `mappend` P.pack xs)
prop_append2BB_monoid xs ys = (xs ++ ys) == (P.unpack $ P.pack xs `mappend` P.pack ys)
prop_append3BB_monoid xs ys = mappend xs ys == P.pack (P.unpack xs ++ P.unpack ys)

prop_append1LL_monoid xs    = (xs ++ xs) == (L.unpack $ L.pack xs `mappend` L.pack xs)
prop_append2LL_monoid xs ys = (xs ++ ys) == (L.unpack $ L.pack xs `mappend` L.pack ys)
prop_append3LL_monoid xs ys = mappend xs ys == L.pack (L.unpack xs ++ L.unpack ys)

prop_map1BB f xs   = P.map f (P.pack xs)    == P.pack (map f xs)
prop_map2BB f g xs = P.map f (P.map g xs) == P.map (f . g) xs
prop_map3BB f xs   = map f xs == (P.unpack . P.map f .  P.pack) xs
-- prop_mapBB' f xs   = P.map' f (P.pack xs) == P.pack (map f xs)

prop_filter1BB (String8 xs) = (filter (=='X') xs) == (C.unpack $ C.filter (=='X') (C.pack xs))
prop_filter2BB p        xs  = (filter p       xs) == (P.unpack $ P.filter p (P.pack xs))

prop_findBB p xs = find p xs == P.find p (P.pack xs)

prop_find_findIndexBB p xs =
    P.find p xs == case P.findIndex p xs of
                                Just n -> Just (xs `P.unsafeIndex` n)
                                _      -> Nothing

prop_foldl1BB xs a = ((foldl (\x c -> if c == a then x else c:x) [] xs)) ==
                   (P.unpack $ P.foldl (\x c -> if c == a then x else c `P.cons` x) P.empty (P.pack xs))
prop_foldl2BB xs = P.foldl (\xs c -> c `P.cons` xs) P.empty (P.pack xs) == P.reverse (P.pack xs)

prop_foldr1BB xs a = ((foldr (\c x -> if c == a then x else c:x) [] xs)) ==
                (P.unpack $ P.foldr (\c x -> if c == a then x else c `P.cons` x)
                    P.empty (P.pack xs))

prop_foldr2BB xs = P.foldr (\c xs -> c `P.cons` xs) P.empty (P.pack xs) == (P.pack xs)

prop_foldl1_1BB xs =
    (not . P.null) xs ==>
    P.foldl1 (\x c -> if c > x then c else x)   xs ==
    P.foldl  (\x c -> if c > x then c else x) 0 xs

prop_foldl1_2BB xs =
    (not . P.null) xs ==>
    P.foldl1 const xs == P.head xs

prop_foldl1_3BB xs =
    (not . P.null) xs ==>
    P.foldl1 (flip const) xs == P.last xs

prop_foldr1_1BB xs =
    (not . P.null) xs ==>
    P.foldr1 (\c x -> if c > x then c else x)   xs ==
    P.foldr  (\c x -> if c > x then c else x) 0 xs

prop_foldr1_2BB xs =
    (not . P.null) xs ==>
    P.foldr1 (flip const) xs == P.last xs

prop_foldr1_3BB xs =
    (not . P.null) xs ==>
    P.foldr1 const xs == P.head xs

prop_takeWhileBB_ne xs a =
  (takeWhile (/= a) xs) == (P.unpack . (P.takeWhile (/= a)) . P.pack) xs
prop_takeWhileBB_eq xs a =
  (takeWhile (== a) xs) == (P.unpack . (P.takeWhile (== a)) . P.pack) xs

prop_dropWhileBB_ne xs a =
  (dropWhile (/= a) xs) == (P.unpack . (P.dropWhile (/= a)) . P.pack) xs
prop_dropWhileBB_eq xs a =
  (dropWhile (== a) xs) == (P.unpack . (P.dropWhile (== a)) . P.pack) xs

prop_dropWhileCC_isSpace (String8 xs) =
        (dropWhile isSpace xs) ==
       (C.unpack .  (C.dropWhile isSpace) . C.pack) xs

prop_takeBB xs = (take 10 xs) == (P.unpack . (P.take 10) . P.pack) xs

prop_dropBB xs = (drop 10 xs) == (P.unpack . (P.drop 10) . P.pack) xs

prop_splitAtBB i xs = -- collect (i >= 0 && i < length xs) $
    splitAt i xs ==
    let (x,y) = P.splitAt i (P.pack xs) in (P.unpack x, P.unpack y)

prop_spanBB xs a = (span (/=a) xs) == (let (x,y) = P.span (/=a) (P.pack xs)
                                     in (P.unpack x, P.unpack y))

prop_breakBB xs a = (break (/=a) xs) == (let (x,y) = P.break (/=a) (P.pack xs)
                                       in (P.unpack x, P.unpack y))

prop_reverse1BB xs = (reverse xs) == (P.unpack . P.reverse . P.pack) xs
prop_reverse2BB xs = P.reverse (P.pack xs) == P.pack (reverse xs)
prop_reverse3BB xs = reverse (P.unpack xs) == (P.unpack . P.reverse) xs

prop_elemBB xs a = (a `elem` xs) == (a `P.elem` (P.pack xs))

prop_notElemBB c xs = P.notElem c (P.pack xs) == notElem c xs

-- should try to stress it
prop_concat1BB xs = (concat [xs,xs]) == (P.unpack $ P.concat [P.pack xs, P.pack xs])
prop_concat2BB xs = (concat [xs,[]]) == (P.unpack $ P.concat [P.pack xs, P.pack []])
prop_concatBB xss = P.concat (map P.pack xss) == P.pack (concat xss)

prop_concat1BB_monoid xs = (concat [xs,xs]) == (P.unpack $ mconcat [P.pack xs, P.pack xs])
prop_concat2BB_monoid xs = (concat [xs,[]]) == (P.unpack $ mconcat [P.pack xs, P.pack []])
prop_concatBB_monoid xss = mconcat (map P.pack xss) == P.pack (concat xss)

prop_concat1LL_monoid xs = (concat [xs,xs]) == (L.unpack $ mconcat [L.pack xs, L.pack xs])
prop_concat2LL_monoid xs = (concat [xs,[]]) == (L.unpack $ mconcat [L.pack xs, L.pack []])
prop_concatLL_monoid xss = mconcat (map L.pack xss) == L.pack (concat xss)

prop_concatMapBB xs = C.concatMap C.singleton xs == (C.pack . concatMap (:[]) . C.unpack) xs

prop_anyBB xs a = (any (== a) xs) == (P.any (== a) (P.pack xs))
prop_allBB xs a = (all (== a) xs) == (P.all (== a) (P.pack xs))

prop_linesBB (String8 xs) =
    (lines xs) == ((map C.unpack) . C.lines . C.pack) xs

prop_unlinesBB (String8 xs) =
    (unlines.lines) xs == (C.unpack. C.unlines . C.lines .C.pack) xs
prop_unlinesLC (String8 xs) =
    (unlines.lines) xs == (LC.unpack. LC.unlines .  LC.lines .LC.pack) xs

prop_lines_lazy1 =
    head (LC.lines (LC.append (LC.pack "a\nb\n") undefined)) == LC.pack "a"
prop_lines_lazy2 =
    head (tail (LC.lines (LC.append (LC.pack "a\nb\n") undefined))) == LC.pack "b"

prop_wordsBB (String8 xs) =
    (words xs) == ((map C.unpack) . C.words . C.pack) xs
-- prop_wordstokensBB xs = C.words xs == C.tokens isSpace xs

prop_unwordsBB (String8 xs) =
    (C.pack.unwords.words) xs == (C.unwords . C.words .C.pack) xs

prop_groupBB xs   = group xs == (map P.unpack . P.group . P.pack) xs

prop_groupByBB  xs = groupBy (==) xs == (map P.unpack . P.groupBy (==) . P.pack) xs
prop_groupBy1BB xs = groupBy (/=) xs == (map P.unpack . P.groupBy (/=) . P.pack) xs
prop_groupBy1CC (String8 xs) = groupBy (==) xs == (map C.unpack . C.groupBy (==) . C.pack) xs
prop_groupBy2CC (String8 xs) = groupBy (/=) xs == (map C.unpack . C.groupBy (/=) . C.pack) xs

prop_joinBB (String8 xs) (String8 ys) =
    (concat . (intersperse ys) . lines) xs ==
    (C.unpack $ C.intercalate (C.pack ys) (C.lines (C.pack xs)))

prop_elemIndex1BB (String8 xs)           = (elemIndex 'X' xs) == (C.elemIndex 'X' (C.pack xs))
prop_elemIndex2BB (String8 xs) (Char8 c) = (elemIndex  c  xs) == (C.elemIndex  c  (C.pack xs))

-- prop_lineIndices1BB xs = C.elemIndices '\n' xs == C.lineIndices xs

prop_countBB c xs = length (P.elemIndices c xs) == P.count c xs

prop_elemIndexEnd1BB c xs =
  P.elemIndexEnd c (P.pack xs) ==
    case P.elemIndex c (P.pack (reverse xs)) of
      Nothing -> Nothing
      Just i  -> Just (length xs - 1 - i)

prop_elemIndexEnd1CC (Char8 c) (String8 xs) =
  C.elemIndexEnd c (C.pack xs) ==
    case C.elemIndex c (C.pack (reverse xs)) of
      Nothing -> Nothing
      Just i  -> Just (length xs - 1 - i)

prop_elemIndexEnd1LL c xs =
  L.elemIndexEnd c (L.pack xs) ==
    case L.elemIndex c (L.pack (reverse xs)) of
      Nothing -> Nothing
      Just i  -> Just (fromIntegral (length xs) - 1 - i)

prop_elemIndexEnd1DD (Char8 c) (String8 xs) =
  D.elemIndexEnd c (D.pack xs) ==
    case D.elemIndex c (D.pack (reverse xs)) of
      Nothing -> Nothing
      Just i  -> Just (fromIntegral (length xs) - 1 - i)

prop_elemIndicesBB xs c = elemIndices c xs == P.elemIndices c (P.pack xs)

prop_findIndexBB xs a = (findIndex (==a) xs) == (P.findIndex (==a) (P.pack xs))

prop_findIndexEndBB xs a = (findIndexEnd (==a) xs) == (P.findIndexEnd (==a) (P.pack xs))

prop_findIndexEndLL xs a = (findIndexEnd (==a) xs) == fmap fromIntegral (L.findIndexEnd (==a) (L.pack xs))

prop_findIndexEndDD (String8 xs) (Char8 a) = (findIndexEnd (==a) xs) == fmap fromIntegral (D.findIndexEnd (==a) (D.pack xs))

prop_findIndiciesBB xs c = (findIndices (==c) xs) == (P.findIndices (==c) (P.pack xs))

-- example properties from QuickCheck.Batch
prop_sort1BB xs = sort xs == (P.unpack . P.sort . P.pack) xs
prop_sort2BB xs = (not (null xs)) ==> (P.head . P.sort . P.pack $ xs) == minimum xs
prop_sort3BB xs = (not (null xs)) ==> (P.last . P.sort . P.pack $ xs) == maximum xs
prop_sort4BB xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (P.head . P.sort) (P.append (P.pack xs) (P.pack ys)) == min (minimum xs) (minimum ys)
prop_sort5BB xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (P.last . P.sort) (P.append (P.pack xs) (P.pack ys)) == max (maximum xs) (maximum ys)

prop_intersperseBB c xs = (intersperse c xs) == (P.unpack $ P.intersperse c (P.pack xs))

-- prop_transposeBB xs = (transpose xs) == ((map P.unpack) . P.transpose .  (map P.pack)) xs

prop_maximumBB xs = (not (null xs)) ==> (maximum xs) == (P.maximum ( P.pack xs ))
prop_minimumBB xs = (not (null xs)) ==> (minimum xs) == (P.minimum ( P.pack xs ))

prop_strip = C.strip `eq1` (C.dropSpace . C.reverse . C.dropSpace . C.reverse)

-- prop_dropSpaceBB xs    = dropWhile isSpace xs == C.unpack (C.dropSpace (C.pack xs))
-- prop_dropSpaceEndBB xs = (C.reverse . (C.dropWhile isSpace) . C.reverse) (C.pack xs) ==
--                        (C.dropSpaceEnd (C.pack xs))

-- prop_breakSpaceBB xs =
--     (let (x,y) = C.breakSpace (C.pack xs)
--      in (C.unpack x, C.unpack y)) == (break isSpace xs)

prop_spanEndBB xs =
        (C.spanEnd (not . isSpace) (C.pack xs)) ==
        (let (x,y) = C.span (not.isSpace) (C.reverse (C.pack xs)) in (C.reverse y,C.reverse x))

prop_breakEndBB p xs = P.breakEnd (not.p) xs == P.spanEnd p xs
prop_breakEndCC p xs = C.breakEnd (not.p) xs == C.spanEnd p xs

{-
prop_breakCharBB c xs =
        (break (==c) xs) ==
        (let (x,y) = C.breakChar c (C.pack xs) in (C.unpack x, C.unpack y))

prop_spanCharBB c xs =
        (break (/=c) xs) ==
        (let (x,y) = C.spanChar c (C.pack xs) in (C.unpack x, C.unpack y))

prop_spanChar_1BB c xs =
        (C.span (==c) xs) == C.spanChar c xs

prop_wordsBB' xs =
    (C.unpack . C.unwords  . C.words' . C.pack) xs ==
    (map (\c -> if isSpace c then ' ' else c) xs)

-- prop_linesBB' xs = (C.unpack . C.unlines' . C.lines' . C.pack) xs == (xs)
-}

prop_unfoldrBB c =
    forAll arbitrarySizedIntegral $ \n ->
      (fst $ C.unfoldrN n fn c) == (C.pack $ take n $ unfoldr fn c)
  where
    fn x = Just (x, if x == maxBound then x else succ x)

prop_prefixBB xs ys = isPrefixOf xs ys == (P.pack xs `P.isPrefixOf` P.pack ys)
prop_prefixLL xs ys = isPrefixOf xs ys == (L.pack xs `L.isPrefixOf` L.pack ys)
prop_suffixBB xs ys = isSuffixOf xs ys == (P.pack xs `P.isSuffixOf` P.pack ys)
prop_suffixLL xs ys = isSuffixOf xs ys == (L.pack xs `L.isSuffixOf` L.pack ys)

prop_stripPrefixBB xs ys = (P.pack <$> stripPrefix xs ys) == (P.pack xs `P.stripPrefix` P.pack ys)
prop_stripPrefixLL xs ys = (L.pack <$> stripPrefix xs ys) == (L.pack xs `L.stripPrefix` L.pack ys)
prop_stripSuffixBB xs ys = (P.pack <$> stripSuffix xs ys) == (P.pack xs `P.stripSuffix` P.pack ys)
prop_stripSuffixLL xs ys = (L.pack <$> stripSuffix xs ys) == (L.pack xs `L.stripSuffix` L.pack ys)

prop_copyBB xs = let p = P.pack xs in P.copy p == p
prop_copyLL xs = let p = L.pack xs in L.copy p == p

prop_initsBB xs = inits xs == map P.unpack (P.inits (P.pack xs))

prop_tailsBB xs = tails xs == map P.unpack (P.tails (P.pack xs))

-- correspondance between break and breakSubstring
prop_breakSubstringBB c l
    = P.break (== c) l == P.breakSubstring (P.singleton c) l

prop_breakSubstring_isInfixOf s l
    = P.isInfixOf s l == if P.null s then True
                                     else case P.breakSubstring s l of
                                            (x,y) | P.null y  -> False
                                                  | otherwise -> True

prop_replicate1BB c = forAll arbitrarySizedIntegral $ \n ->
                      P.unpack (P.replicate n c) == replicate n c
prop_replicate2BB c = forAll arbitrarySizedIntegral $ \n ->
                      P.replicate n c == fst (P.unfoldrN n (\u -> Just (u,u)) c)

prop_replicate3BB c = P.unpack (P.replicate 0 c) == replicate 0 c

prop_readintBB n = (fst . fromJust . C.readInt . C.pack . show) n == (n :: Int)
prop_readintLL n = (fst . fromJust . D.readInt . D.pack . show) n == (n :: Int)

prop_readBB x = (read . show) x == (x :: P.ByteString)
prop_readLL x = (read . show) x == (x :: L.ByteString)

prop_readint2BB (String8 s) =
    let s' = filter (\c -> c `notElem` ['0'..'9']) s
    in C.readInt (C.pack s') == Nothing

prop_readintegerBB n = (fst . fromJust . C.readInteger . C.pack . show) n == (n :: Integer)
prop_readintegerLL n = (fst . fromJust . D.readInteger . D.pack . show) n == (n :: Integer)

prop_readinteger2BB (String8 s) =
    let s' = filter (\c -> c `notElem` ['0'..'9']) s
    in C.readInteger (C.pack s') == Nothing


-- Ensure that readInt and readInteger over lazy ByteStrings are not
-- excessively strict.
prop_readIntSafe         = (fst . fromJust . D.readInt) (Chunk (C.pack "1z") Empty)         == 1
prop_readIntUnsafe       = (fst . fromJust . D.readInt) (Chunk (C.pack "2z") undefined)     == 2
prop_readIntegerSafe     = (fst . fromJust . D.readInteger) (Chunk (C.pack "1z") Empty)     == 1
prop_readIntegerUnsafe   = (fst . fromJust . D.readInteger) (Chunk (C.pack "2z") undefined) == 2

-- prop_filterChar1BB c xs = (filter (==c) xs) == ((C.unpack . C.filterChar c . C.pack) xs)
-- prop_filterChar2BB c xs = (C.filter (==c) (C.pack xs)) == (C.filterChar c (C.pack xs))
-- prop_filterChar3BB c xs = C.filterChar c xs == C.replicate (C.count c xs) c

-- prop_filterNotChar1BB c xs = (filter (/=c) xs) == ((C.unpack . C.filterNotChar c . C.pack) xs)
-- prop_filterNotChar2BB c xs = (C.filter (/=c) (C.pack xs)) == (C.filterNotChar c (C.pack xs))

-- prop_joinjoinpathBB xs ys c = C.joinWithChar c xs ys == C.join (C.singleton c) [xs,ys]

prop_zipBB  xs ys = zip xs ys == P.zip (P.pack xs) (P.pack ys)
prop_zipLC (String8 xs) (String8 ys)
                  = zip xs ys == LC.zip (LC.pack xs) (LC.pack ys)
prop_zip1BB xs ys = P.zip xs ys == zip (P.unpack xs) (P.unpack ys)

prop_zipWithBB xs ys = P.zipWith (,) xs ys == P.zip xs ys
prop_zipWithCC xs ys = C.zipWith (,) xs ys == C.zip xs ys
prop_zipWithLC xs ys = LC.zipWith (,) xs ys == LC.zip xs ys

prop_packZipWithBB f xs ys = P.pack (P.zipWith f xs ys) == P.packZipWith f xs ys
prop_packZipWithLL f xs ys = L.pack (L.zipWith f xs ys) == L.packZipWith f xs ys
prop_packZipWithBC f xs ys = C.pack (C.zipWith f xs ys) == C.packZipWith f xs ys
prop_packZipWithLC f xs ys = LC.pack (LC.zipWith f xs ys) == LC.packZipWith f xs ys


prop_unzipBB x = let (xs,ys) = unzip x in (P.pack xs, P.pack ys) == P.unzip x

#if MIN_VERSION_base(4,9,0)
prop_stimesBB :: NonNegative Int -> P.ByteString -> Bool
prop_stimesBB (NonNegative i) bs = stimes i bs == mtimesDefault i bs

prop_stimesLL :: NonNegative Int -> L.ByteString -> Bool
prop_stimesLL (NonNegative i) bs = stimes i bs == mtimesDefault i bs
#endif

-- prop_zipwith_spec f p q =
--   P.pack (P.zipWith f p q) == P.zipWith' f p q
--   where _ = f :: Word8 -> Word8 -> Word8

-- prop_join_spec c s1 s2 =
--  P.join (P.singleton c) (s1 : s2 : []) == P.joinWithByte c s1 s2

------------------------------------------------------------------------

-- Test IsString, Show, Read, pack, unpack
prop_isstring    :: String8 -> Bool
prop_isstring_lc :: String8 -> Bool

prop_isstring    (String8 x) = C.unpack  (fromString x :: C.ByteString) == x
prop_isstring_lc (String8 x) = LC.unpack (fromString x :: LC.ByteString) == x

prop_showP1 x = show x == show (C.unpack x)
prop_showL1 x = show x == show (LC.unpack x)

prop_readP1 x = read (show x) == (x :: P.ByteString)
prop_readP2 x = read (show x) == C.pack (x :: String)

prop_readL1 x = read (show x) == (x :: L.ByteString)
prop_readL2 x = read (show x) == LC.pack (x :: String)

prop_packunpack_s x = (P.unpack . P.pack) x == x
prop_unpackpack_s x = (P.pack . P.unpack) x == x

prop_packunpack_c (String8 x) = (C.unpack . C.pack) x == x
prop_unpackpack_c          x  = (C.pack . C.unpack) x == x

prop_packunpack_l x = (L.unpack . L.pack) x == x
prop_unpackpack_l x = (L.pack . L.unpack) x == x

prop_packunpack_lc (String8 x) = (LC.unpack . LC.pack) x == x
prop_unpackpack_lc          x  = (LC.pack . LC.unpack) x == x

prop_toFromChunks x = (L.fromChunks . L.toChunks) x == x
prop_fromToChunks x = (L.toChunks . L.fromChunks) x == filter (not . P.null) x

prop_toFromStrict x = (L.fromStrict . L.toStrict) x == x
prop_fromToStrict x = (L.toStrict . L.fromStrict) x == x

prop_packUptoLenBytes cs =
    forAll (choose (0, length cs + 1)) $ \n ->
      let (bs, cs') = P.packUptoLenBytes n cs
       in P.length bs == min n (length cs)
       && take n cs == P.unpack bs
       && P.pack (take n cs) == bs
       && drop n cs == cs'

prop_packUptoLenChars (String8 cs) =
    forAll (choose (0, length cs + 1)) $ \n ->
      let (bs, cs') = P.packUptoLenChars n cs
       in P.length bs == min n (length cs)
       && take n cs == C.unpack bs
       && C.pack (take n cs) == bs
       && drop n cs == cs'

prop_unpack_s cs =
    forAll (choose (0, length cs)) $ \n ->
      P.unpack (P.drop n $ P.pack cs) == drop n cs
prop_unpack_c (String8 cs) =
    forAll (choose (0, length cs)) $ \n ->
      C.unpack (C.drop n $ C.pack cs) == drop n cs

prop_unpack_l  cs =
    forAll (choose (0, length cs)) $ \n ->
      L.unpack (L.drop (fromIntegral n) $ L.pack cs) == drop n cs
prop_unpack_lc (String8 cs) =
    forAll (choose (0, length cs)) $ \n ->
      LC.unpack (L.drop (fromIntegral n) $ LC.pack cs) == drop n cs

prop_unpackBytes cs =
    forAll (choose (0, length cs)) $ \n ->
      P.unpackBytes (P.drop n $ P.pack cs) == drop n cs
prop_unpackChars (String8 cs) =
    forAll (choose (0, length cs)) $ \n ->
      P.unpackChars (P.drop n $ C.pack cs) == drop n cs

prop_unpackBytes_l =
    forAll (sized $ \n -> resize (n * 10) arbitrary) $ \cs ->
    forAll (choose (0, length cs)) $ \n ->
      L.unpackBytes (L.drop (fromIntegral n) $ L.pack cs) == drop n cs
prop_unpackChars_l =
    forAll (sized $ \n -> resize (n * 10) arbitrary) $ \(String8 cs) ->
    forAll (choose (0, length cs)) $ \n ->
      L.unpackChars (L.drop (fromIntegral n) $ LC.pack cs) == drop n cs

prop_unpackAppendBytesLazy cs' =
    forAll (sized $ \n -> resize (n * 10) arbitrary) $ \cs ->
    forAll (choose (0, 2)) $ \n ->
      P.unpackAppendBytesLazy (P.drop n $ P.pack cs) cs' == drop n cs ++ cs'
prop_unpackAppendCharsLazy (String8 cs') =
    forAll (sized $ \n -> resize (n * 10) arbitrary) $ \(String8 cs) ->
    forAll (choose (0, 2)) $ \n ->
      P.unpackAppendCharsLazy (P.drop n $ C.pack cs) cs' == drop n cs ++ cs'

prop_unpackAppendBytesStrict cs cs' =
    forAll (choose (0, length cs)) $ \n ->
      P.unpackAppendBytesStrict (P.drop n $ P.pack cs) cs' == drop n cs ++ cs'

prop_unpackAppendCharsStrict (String8 cs) (String8 cs') =
    forAll (choose (0, length cs)) $ \n ->
      P.unpackAppendCharsStrict (P.drop n $ C.pack cs) cs' == drop n cs ++ cs'

------------------------------------------------------------------------
-- Unsafe functions

-- Test unsafePackAddress
prop_unsafePackAddress (CByteString x) = ioProperty $ do
        let (p,_,_) = P.toForeignPtr (x `P.snoc` 0)
        y <- withForeignPtr p $ \(Ptr addr) ->
            P.unsafePackAddress addr
        return (y == x)

-- Test unsafePackAddressLen
prop_unsafePackAddressLen x = ioProperty $ do
        let i = P.length x
            (p,_,_) = P.toForeignPtr (x `P.snoc` 0)
        y <- withForeignPtr p $ \(Ptr addr) ->
            P.unsafePackAddressLen i addr
        return (y == x)

prop_unsafeUseAsCString x = ioProperty $ do
        let n = P.length x
        y <- P.unsafeUseAsCString x $ \cstr ->
                    sequence [ do a <- peekElemOff cstr i
                                  let b = x `P.index` i
                                  return (a == fromIntegral b)
                             | i <- [0.. n-1]     ]
        return (and y)

prop_unsafeUseAsCStringLen x = ioProperty $ do
        let n = P.length x
        y <- P.unsafeUseAsCStringLen x $ \(cstr,_) ->
                    sequence [ do a <- peekElemOff cstr i
                                  let b = x `P.index` i
                                  return (a == fromIntegral b)
                             | i <- [0.. n-1]     ]
        return (and y)

prop_internal_invariant x = L.invariant x

prop_useAsCString x = ioProperty $ do
        let n = P.length x
        y <- P.useAsCString x $ \cstr ->
                    sequence [ do a <- peekElemOff cstr i
                                  let b = x `P.index` i
                                  return (a == fromIntegral b)
                             | i <- [0.. n-1]     ]
        return (and y)

prop_packCString (CByteString x) = ioProperty $ do
        y <- P.useAsCString x $ P.unsafePackCString
        return (y == x)

prop_packCString_safe (CByteString x) = ioProperty $ do
        y <- P.useAsCString x $ P.packCString
        return (y == x)

prop_packCStringLen x = ioProperty $ do
        y <- P.useAsCStringLen x $ P.unsafePackCStringLen
        return (y == x && P.length y == P.length x)

prop_packCStringLen_safe x = ioProperty $ do
        y <- P.useAsCStringLen x $ P.packCStringLen
        return (y == x && P.length y == P.length x)

prop_packMallocCString (CByteString x) = ioProperty $ do

         let (fp,_,_) = P.toForeignPtr x
         ptr <- mallocArray0 (P.length x) :: IO (Ptr Word8)
         forM_ [0 .. P.length x] $ \n -> pokeElemOff ptr n 0
         withForeignPtr fp $ \qtr -> copyArray ptr qtr (P.length x)
         y   <- P.unsafePackMallocCString (castPtr ptr)

         let !z = y == x
         free ptr `seq` return z

prop_unsafeFinalize    x =
    P.length x > 0 ==>
      ioProperty $ do
        x <- P.unsafeFinalize x
        return (x == ())

prop_packCStringFinaliser x = ioProperty $ do
        y <- P.useAsCString x $ \cstr -> P.unsafePackCStringFinalizer (castPtr cstr) (P.length x) (return ())
        return (y == x)

prop_fromForeignPtr x = (let (a,b,c) = (P.toForeignPtr x)
                                in P.fromForeignPtr a b c) == x

------------------------------------------------------------------------
-- IO

prop_read_write_file_P x = ioProperty $ do
    (fn, h) <- openTempFile "." "prop-compiled.tmp"
    hClose h
    P.writeFile fn x
    y <- P.readFile fn
    removeFile fn
    return (x == y)

prop_read_write_file_C x = ioProperty $ do
    (fn, h) <- openTempFile "." "prop-compiled.tmp"
    hClose h
    C.writeFile fn x
    y <- C.readFile fn
    removeFile fn
    return (x == y)

prop_read_write_file_L x = ioProperty $ do
    (fn, h) <- openTempFile "." "prop-compiled.tmp"
    hClose h
    L.writeFile fn x
    y <- L.readFile fn
    L.length y `seq` removeFile fn
    return (x == y)

prop_read_write_file_D x = ioProperty $ do
    (fn, h) <- openTempFile "." "prop-compiled.tmp"
    hClose h
    D.writeFile fn x
    y <- D.readFile fn
    D.length y `seq` removeFile fn
    return (x == y)

------------------------------------------------------------------------

prop_append_file_P x y = ioProperty $ do
    (fn, h) <- openTempFile "." "prop-compiled.tmp"
    hClose h
    P.writeFile fn x
    P.appendFile fn y
    z <- P.readFile fn
    removeFile fn
    return (z == x `P.append` y)

prop_append_file_C x y = ioProperty $ do
    (fn, h) <- openTempFile "." "prop-compiled.tmp"
    hClose h
    C.writeFile fn x
    C.appendFile fn y
    z <- C.readFile fn
    removeFile fn
    return (z == x `C.append` y)

prop_append_file_L x y = ioProperty $ do
    (fn, h) <- openTempFile "." "prop-compiled.tmp"
    hClose h
    L.writeFile fn x
    L.appendFile fn y
    z <- L.readFile fn
    L.length y `seq` removeFile fn
    return (z == x `L.append` y)

prop_append_file_D x y = ioProperty $ do
    (fn, h) <- openTempFile "." "prop-compiled.tmp"
    hClose h
    D.writeFile fn x
    D.appendFile fn y
    z <- D.readFile fn
    D.length y `seq` removeFile fn
    return (z == x `D.append` y)

prop_packAddress = C.pack "this is a test"
            ==
                   C.pack "this is a test"

prop_isSpaceWord8 w = isSpace c == P.isSpaceChar8 c
   where c = chr (fromIntegral (w :: Word8))


------------------------------------------------------------------------
-- ByteString.Short
--

prop_short_pack_unpack xs =
    (Short.unpack . Short.pack) xs == xs
prop_short_toShort_fromShort bs =
    (Short.fromShort . Short.toShort) bs == bs

prop_short_toShort_unpack bs =
    (Short.unpack . Short.toShort) bs == P.unpack bs
prop_short_pack_fromShort xs =
    (Short.fromShort . Short.pack) xs == P.pack xs

prop_short_empty =
    Short.empty == Short.toShort P.empty
 && Short.empty == Short.pack []
 && Short.null (Short.toShort P.empty)
 && Short.null (Short.pack [])
 && Short.null Short.empty

prop_short_null_toShort bs =
    P.null bs == Short.null (Short.toShort bs)
prop_short_null_pack xs =
    null xs == Short.null (Short.pack xs)

prop_short_length_toShort bs =
    P.length bs == Short.length (Short.toShort bs)
prop_short_length_pack xs =
    length xs == Short.length (Short.pack xs)

prop_short_index_pack xs =
    all (\i -> Short.pack xs `Short.index` i == xs !! i)
        [0 .. length xs - 1]
prop_short_index_toShort bs =
    all (\i -> Short.toShort bs `Short.index` i == bs `P.index` i)
        [0 .. P.length bs - 1]

prop_short_eq xs ys =
    (xs == ys) == (Short.pack xs == Short.pack ys)
prop_short_ord xs ys =
    (xs `compare` ys) == (Short.pack xs `compare` Short.pack ys)

prop_short_mappend_empty_empty =
    Short.empty `mappend` Short.empty  == Short.empty
prop_short_mappend_empty xs =
    Short.empty `mappend` Short.pack xs == Short.pack xs
 && Short.pack xs `mappend` Short.empty == Short.pack xs
prop_short_mappend xs ys =
    (xs `mappend` ys) == Short.unpack (Short.pack xs `mappend` Short.pack ys)
prop_short_mconcat xss =
    mconcat xss == Short.unpack (mconcat (map Short.pack xss))

prop_short_fromString s =
    fromString s == Short.fromShort (fromString s)

prop_short_show xs =
    show (Short.pack xs) == show (map P.w2c xs)
prop_short_show' xs =
    show (Short.pack xs) == show (P.pack xs)

prop_short_read xs =
    read (show (Short.pack xs)) == Short.pack xs

prop_short_pinned :: NonNegative Int -> Property
prop_short_pinned (NonNegative (I# len#)) = runST $ ST $ \s ->
  case newPinnedByteArray# len# s of
    (# s', mba# #) -> case unsafeFreezeByteArray# mba# s' of
      (# s'', ba# #) -> let sbs = Short.SBS ba# in
        (# s'', sbs === Short.toShort (Short.fromShort sbs) #)

stripSuffix :: [W] -> [W] -> Maybe [W]
stripSuffix xs ys = reverse <$> stripPrefix (reverse xs) (reverse ys)

short_tests =
    [ testProperty "pack/unpack"              prop_short_pack_unpack
    , testProperty "toShort/fromShort"        prop_short_toShort_fromShort
    , testProperty "toShort/unpack"           prop_short_toShort_unpack
    , testProperty "pack/fromShort"           prop_short_pack_fromShort
    , testProperty "empty"                    prop_short_empty
    , testProperty "null/toShort"             prop_short_null_toShort
    , testProperty "null/pack"                prop_short_null_pack
    , testProperty "length/toShort"           prop_short_length_toShort
    , testProperty "length/pack"              prop_short_length_pack
    , testProperty "index/pack"               prop_short_index_pack
    , testProperty "index/toShort"            prop_short_index_toShort
    , testProperty "Eq"                       prop_short_eq
    , testProperty "Ord"                      prop_short_ord
    , testProperty "mappend/empty/empty"      prop_short_mappend_empty_empty
    , testProperty "mappend/empty"            prop_short_mappend_empty
    , testProperty "mappend"                  prop_short_mappend
    , testProperty "mconcat"                  prop_short_mconcat
    , testProperty "fromString"               prop_short_fromString
    , testProperty "show"                     prop_short_show
    , testProperty "show'"                    prop_short_show'
    , testProperty "read"                     prop_short_read
    , testProperty "pinned"                   prop_short_pinned
    ]

------------------------------------------------------------------------
-- The entry point

main :: IO ()
main = defaultMain $ testGroup "All" tests

--
-- And now a list of all the properties to test.
--

tests = misc_tests
     ++ bl_tests
     ++ cc_tests
     ++ bp_tests
     ++ pl_tests
     ++ bb_tests
     ++ ll_tests
     ++ io_tests
     ++ short_tests
     ++ rules

--
-- 'morally sound' IO
--
io_tests =
    [ testProperty "readFile.writeFile" prop_read_write_file_P
    , testProperty "readFile.writeFile" prop_read_write_file_C
    , testProperty "readFile.writeFile" prop_read_write_file_L
    , testProperty "readFile.writeFile" prop_read_write_file_D

    , testProperty "appendFile        " prop_append_file_P
    , testProperty "appendFile        " prop_append_file_C
    , testProperty "appendFile        " prop_append_file_L
    , testProperty "appendFile        " prop_append_file_D

    , testProperty "packAddress       " prop_packAddress

    ]

misc_tests =
    [ testProperty "packunpack (bytes)"     prop_packunpack_s
    , testProperty "unpackpack (bytes)"     prop_unpackpack_s
    , testProperty "packunpack (chars)"     prop_packunpack_c
    , testProperty "unpackpack (chars)"     prop_unpackpack_c
    , testProperty "packunpack (lazy bytes)" prop_packunpack_l
    , testProperty "unpackpack (lazy bytes)" prop_unpackpack_l
    , testProperty "packunpack (lazy chars)" prop_packunpack_lc
    , testProperty "unpackpack (lazy chars)" prop_unpackpack_lc
    , testProperty "unpack (bytes)"         prop_unpack_s
    , testProperty "unpack (chars)"         prop_unpack_c
    , testProperty "unpack (lazy bytes)"    prop_unpack_l
    , testProperty "unpack (lazy chars)"    prop_unpack_lc
    , testProperty "packUptoLenBytes"       prop_packUptoLenBytes
    , testProperty "packUptoLenChars"       prop_packUptoLenChars
    , testProperty "unpackBytes"            prop_unpackBytes
    , testProperty "unpackChars"            prop_unpackChars
    , testProperty "unpackBytes"            prop_unpackBytes_l
    , testProperty "unpackChars"            prop_unpackChars_l
    , testProperty "unpackAppendBytesLazy"  prop_unpackAppendBytesLazy
    , testProperty "unpackAppendCharsLazy"  prop_unpackAppendCharsLazy
    , testProperty "unpackAppendBytesStrict"prop_unpackAppendBytesStrict
    , testProperty "unpackAppendCharsStrict"prop_unpackAppendCharsStrict
    , testProperty "toFromChunks"           prop_toFromChunks
    , testProperty "fromToChunks"           prop_fromToChunks
    , testProperty "toFromStrict"           prop_toFromStrict
    , testProperty "fromToStrict"           prop_fromToStrict

    , testProperty "invariant"              prop_invariant
    , testProperty "unsafe pack address"    prop_unsafePackAddress
    , testProperty "unsafe pack address len"prop_unsafePackAddressLen
    , testProperty "unsafeUseAsCString"     prop_unsafeUseAsCString
    , testProperty "unsafeUseAsCStringLen"  prop_unsafeUseAsCStringLen
    , testProperty "useAsCString"           prop_useAsCString
    , testProperty "packCString"            prop_packCString
    , testProperty "packCString safe"       prop_packCString_safe
    , testProperty "packCStringLen"         prop_packCStringLen
    , testProperty "packCStringLen safe"    prop_packCStringLen_safe
    , testProperty "packCStringFinaliser"   prop_packCStringFinaliser
    , testProperty "packMallocString"       prop_packMallocCString
    , testProperty "unsafeFinalise"         prop_unsafeFinalize
    , testProperty "invariant"              prop_internal_invariant
    , testProperty "show 1"                 prop_showP1
    , testProperty "show 2"                 prop_showL1
    , testProperty "read 1"                 prop_readP1
    , testProperty "read 2"                 prop_readP2
    , testProperty "read 3"                 prop_readL1
    , testProperty "read 4"                 prop_readL2
    , testProperty "fromForeignPtr"         prop_fromForeignPtr
    ]

------------------------------------------------------------------------
-- ByteString.Lazy <=> List

bl_tests =
    [ testProperty "all"         prop_allBL
    , testProperty "any"         prop_anyBL
    , testProperty "append"      prop_appendBL
    , testProperty "compare"     prop_compareBL
    , testProperty "concat"      prop_concatBL
    , testProperty "cons"        prop_consBL
    , testProperty "eq"          prop_eqBL
    , testProperty "filter"      prop_filterBL
    , testProperty "find"        prop_findBL
    , testProperty "findIndex"   prop_findIndexBL
    , testProperty "findIndexEnd"prop_findIndexEndBL
    , testProperty "findIndices" prop_findIndicesBL
    , testProperty "foldl"       prop_foldlBL
    , testProperty "foldl'"      prop_foldlBL'
    , testProperty "foldl1"      prop_foldl1BL
    , testProperty "foldl1'"     prop_foldl1BL'
    , testProperty "foldr"       prop_foldrBL
    , testProperty "foldr1"      prop_foldr1BL
    , testProperty "mapAccumL"   prop_mapAccumLBL
    , testProperty "mapAccumR"   prop_mapAccumRBL
    , testProperty "mapAccumR"   prop_mapAccumRDL
    , testProperty "mapAccumR"   prop_mapAccumRCC
    , testProperty "unfoldr"     prop_unfoldrBL
    , testProperty "unfoldr"     prop_unfoldrLC
    , testProperty "unfoldr"     prop_cycleLC
    , testProperty "iterate"     prop_iterateLC
    , testProperty "iterate"     prop_iterateLC_2
    , testProperty "iterate"     prop_iterateL
    , testProperty "repeat"      prop_repeatLC
    , testProperty "repeat"      prop_repeatL
    , testProperty "head"        prop_headBL
    , testProperty "init"        prop_initBL
    , testProperty "isPrefixOf"  prop_isPrefixOfBL
    , testProperty "isSuffixOf"  prop_isSuffixOfBL
    , testProperty "stripPrefix" prop_stripPrefixBL
    , testProperty "stripSuffix" prop_stripSuffixBL
    , testProperty "last"        prop_lastBL
    , testProperty "length"      prop_lengthBL
    , testProperty "map"         prop_mapBL
    , testProperty "maximum"     prop_maximumBL
    , testProperty "minimum"     prop_minimumBL
    , testProperty "null"        prop_nullBL
    , testProperty "reverse"     prop_reverseBL
    , testProperty "snoc"        prop_snocBL
    , testProperty "tail"        prop_tailBL
    , testProperty "transpose"   prop_transposeBL
    , testProperty "replicate"   prop_replicateBL
    , testProperty "take"        prop_takeBL
    , testProperty "drop"        prop_dropBL
    , testProperty "splitAt"     prop_splitAtBL
    , testProperty "takeWhile"   prop_takeWhileBL
    , testProperty "dropWhile"   prop_dropWhileBL
    , testProperty "break"       prop_breakBL
    , testProperty "span"        prop_spanBL
    , testProperty "group"       prop_groupBL
    , testProperty "groupBy"     prop_groupByBL
    , testProperty "inits"       prop_initsBL
    , testProperty "tails"       prop_tailsBL
    , testProperty "elem"        prop_elemBL
    , testProperty "notElem"     prop_notElemBL
    , testProperty "lines"       prop_linesBL
    , testProperty "elemIndex"   prop_elemIndexBL
    , testProperty "elemIndexEnd"prop_elemIndexEndBL
    , testProperty "elemIndices" prop_elemIndicesBL
    , testProperty "concatMap"   prop_concatMapBL
    , testProperty "zipWith/packZipWithLazy" prop_packZipWithBL
    ]

------------------------------------------------------------------------
-- ByteString.Lazy <=> ByteString

cc_tests =
    [ testProperty "prop_concatCC"      prop_concatCC
    , testProperty "prop_nullCC"        prop_nullCC
    , testProperty "prop_reverseCC"     prop_reverseCC
    , testProperty "prop_transposeCC"   prop_transposeCC
    , testProperty "prop_groupCC"       prop_groupCC
    , testProperty "prop_groupByCC"     prop_groupByCC
    , testProperty "prop_initsCC"       prop_initsCC
    , testProperty "prop_tailsCC"       prop_tailsCC
    , testProperty "prop_allCC"         prop_allCC
    , testProperty "prop_anyCC"         prop_anyCC
    , testProperty "prop_appendCC"      prop_appendCC
    , testProperty "prop_breakCC"       prop_breakCC
    , testProperty "prop_concatMapCC"   prop_concatMapCC
    , testProperty "prop_consCC"        prop_consCC
    , testProperty "prop_consCC'"       prop_consCC'
    , testProperty "prop_unconsCC"      prop_unconsCC
    , testProperty "prop_unsnocCC"      prop_unsnocCC
    , testProperty "prop_countCC"       prop_countCC
    , testProperty "prop_dropCC"        prop_dropCC
    , testProperty "prop_dropWhileCC"   prop_dropWhileCC
    , testProperty "prop_filterCC"      prop_filterCC
    , testProperty "prop_findCC"        prop_findCC
    , testProperty "prop_findIndexCC"   prop_findIndexCC
    , testProperty "prop_findIndexEndCC" prop_findIndexEndCC
    , testProperty "prop_findIndicesCC" prop_findIndicesCC
    , testProperty "prop_isPrefixCC"    prop_isPrefixOfCC
    , testProperty "prop_isSuffixCC"    prop_isSuffixOfCC
    , testProperty "prop_stripPrefixCC" prop_stripPrefixCC
    , testProperty "prop_stripSuffixCC" prop_stripSuffixCC
    , testProperty "prop_mapCC"         prop_mapCC
    , testProperty "prop_replicateCC"   prop_replicateCC
    , testProperty "prop_snocCC"        prop_snocCC
    , testProperty "prop_spanCC"        prop_spanCC
    , testProperty "prop_splitCC"       prop_splitCC
    , testProperty "prop_splitAtCC"     prop_splitAtCC
    , testProperty "prop_takeCC"        prop_takeCC
    , testProperty "prop_takeWhileCC"   prop_takeWhileCC
    , testProperty "prop_elemCC"        prop_elemCC
    , testProperty "prop_notElemCC"     prop_notElemCC
    , testProperty "prop_elemIndexCC"   prop_elemIndexCC
    , testProperty "prop_elemIndicesCC" prop_elemIndicesCC
    , testProperty "prop_lengthCC"      prop_lengthCC
    , testProperty "prop_headCC"        prop_headCC
    , testProperty "prop_initCC"        prop_initCC
    , testProperty "prop_lastCC"        prop_lastCC
    , testProperty "prop_maximumCC"     prop_maximumCC
    , testProperty "prop_minimumCC"     prop_minimumCC
    , testProperty "prop_tailCC"        prop_tailCC
    , testProperty "prop_foldl1CC"      prop_foldl1CC
    , testProperty "prop_foldl1CC'"     prop_foldl1CC'
    , testProperty "prop_foldr1CC"      prop_foldr1CC
    , testProperty "prop_foldr1CC'"     prop_foldr1CC'
    , testProperty "prop_scanlCC"       prop_scanlCC
    , testProperty "prop_intersperseCC" prop_intersperseCC

    , testProperty "prop_foldlCC"       prop_foldlCC
    , testProperty "prop_foldlCC'"      prop_foldlCC'
    , testProperty "prop_foldrCC"       prop_foldrCC
    , testProperty "prop_foldrCC'"      prop_foldrCC'
    , testProperty "prop_mapAccumLCC"   prop_mapAccumLCC
--    , testProperty "prop_mapIndexedCC" prop_mapIndexedCC
--    , testProperty "prop_mapIndexedPL" prop_mapIndexedPL
    ]

bp_tests =
    [ testProperty "all"         prop_allBP
    , testProperty "any"         prop_anyBP
    , testProperty "append"      prop_appendBP
    , testProperty "compare"     prop_compareBP
    , testProperty "concat"      prop_concatBP
    , testProperty "cons"        prop_consBP
    , testProperty "cons'"       prop_consBP'
    , testProperty "uncons"      prop_unconsBP
    , testProperty "unsnoc"      prop_unsnocBP
    , testProperty "eq"          prop_eqBP
    , testProperty "filter"      prop_filterBP
    , testProperty "find"        prop_findBP
    , testProperty "findIndex"   prop_findIndexBP
    , testProperty "findIndexEnd"prop_findIndexEndBP
    , testProperty "findIndices" prop_findIndicesBP
    , testProperty "foldl"       prop_foldlBP
    , testProperty "foldl'"      prop_foldlBP'
    , testProperty "foldl1"      prop_foldl1BP
    , testProperty "foldl1'"     prop_foldl1BP'
    , testProperty "foldr"       prop_foldrBP
    , testProperty "foldr'"      prop_foldrBP'
    , testProperty "foldr1"      prop_foldr1BP
    , testProperty "foldr1'"     prop_foldr1BP'
    , testProperty "mapAccumL"   prop_mapAccumLBP
--  , testProperty "mapAccumL"   prop_mapAccumL_mapIndexedBP
    , testProperty "unfoldr"     prop_unfoldrBP
    , testProperty "unfoldr 2"   prop_unfoldr2BP
    , testProperty "unfoldr 2"   prop_unfoldr2CP
    , testProperty "head"        prop_headBP
    , testProperty "init"        prop_initBP
    , testProperty "isPrefixOf"  prop_isPrefixOfBP
    , testProperty "isSuffixOf"  prop_isSuffixOfBP
    , testProperty "stripPrefix" prop_stripPrefixBP
    , testProperty "stripSuffix" prop_stripSuffixBP
    , testProperty "last"        prop_lastBP
    , testProperty "length"      prop_lengthBP
    , testProperty "readInt"     prop_readIntBP
    , testProperty "lines"       prop_linesBP
    , testProperty "lines \\n"   prop_linesNLBP
    , testProperty "map"         prop_mapBP
    , testProperty "maximum   "  prop_maximumBP
    , testProperty "minimum"     prop_minimumBP
    , testProperty "null"        prop_nullBP
    , testProperty "reverse"     prop_reverseBP
    , testProperty "snoc"        prop_snocBP
    , testProperty "tail"        prop_tailBP
    , testProperty "scanl"       prop_scanlBP
    , testProperty "transpose"   prop_transposeBP
    , testProperty "replicate"   prop_replicateBP
    , testProperty "take"        prop_takeBP
    , testProperty "drop"        prop_dropBP
    , testProperty "splitAt"     prop_splitAtBP
    , testProperty "takeWhile"   prop_takeWhileBP
    , testProperty "dropWhile"   prop_dropWhileBP
    , testProperty "break"       prop_breakBP
    , testProperty "span"        prop_spanBP
    , testProperty "split"       prop_splitBP
    , testProperty "count"       prop_countBP
    , testProperty "group"       prop_groupBP
    , testProperty "groupBy"     prop_groupByBP
    , testProperty "inits"       prop_initsBP
    , testProperty "tails"       prop_tailsBP
    , testProperty "elem"        prop_elemBP
    , testProperty "notElem"     prop_notElemBP
    , testProperty "elemIndex"   prop_elemIndexBP
    , testProperty "elemIndexEnd"prop_elemIndexEndBP
    , testProperty "elemIndices" prop_elemIndicesBP
    , testProperty "intersperse" prop_intersperseBP
    , testProperty "concatMap"   prop_concatMapBP
    ]

------------------------------------------------------------------------
-- ByteString <=> List

pl_tests =
    [ testProperty "all"         prop_allPL
    , testProperty "any"         prop_anyPL
    , testProperty "append"      prop_appendPL
    , testProperty "compare"     prop_comparePL
    , testProperty "concat"      prop_concatPL
    , testProperty "cons"        prop_consPL
    , testProperty "eq"          prop_eqPL
    , testProperty "filter"      prop_filterPL
    , testProperty "filter rules"prop_filterPL_rule
    , testProperty "filter rules"prop_filterLC_rule
    , testProperty "partition"   prop_partitionPL
    , testProperty "partition"   prop_partitionLL
    , testProperty "find"        prop_findPL
    , testProperty "findIndex"   prop_findIndexPL
    , testProperty "findIndexEnd"prop_findIndexEndPL
    , testProperty "findIndices" prop_findIndicesPL
    , testProperty "foldl"       prop_foldlPL
    , testProperty "foldl'"      prop_foldlPL'
    , testProperty "foldl1"      prop_foldl1PL
    , testProperty "foldl1'"     prop_foldl1PL'
    , testProperty "foldr1"      prop_foldr1PL
    , testProperty "foldr"       prop_foldrPL
    , testProperty "mapAccumL"   prop_mapAccumLPL
    , testProperty "mapAccumR"   prop_mapAccumRPL
    , testProperty "unfoldr"     prop_unfoldrPL
    , testProperty "scanl"       prop_scanlPL
    , testProperty "scanl1"      prop_scanl1PL
    , testProperty "scanl1"      prop_scanl1CL
    , testProperty "scanr"       prop_scanrCL
    , testProperty "scanr"       prop_scanrPL
    , testProperty "scanr1"      prop_scanr1PL
    , testProperty "scanr1"      prop_scanr1CL
    , testProperty "head"        prop_headPL
    , testProperty "init"        prop_initPL
    , testProperty "last"        prop_lastPL
    , testProperty "maximum"     prop_maximumPL
    , testProperty "minimum"     prop_minimumPL
    , testProperty "tail"        prop_tailPL
    , testProperty "zip"         prop_zipPL
    , testProperty "zip"         prop_zipLL
    , testProperty "zip"         prop_zipCL
    , testProperty "unzip"       prop_unzipPL
    , testProperty "unzip"       prop_unzipLL
    , testProperty "unzip"       prop_unzipCL
    , testProperty "unzip"       prop_unzipDL
    , testProperty "zipWithPL"          prop_zipWithPL
    , testProperty "zipWithPL rules"   prop_zipWithPL_rules
    , testProperty "packZipWithPL" prop_packZipWithPL

    , testProperty "isPrefixOf"  prop_isPrefixOfPL
    , testProperty "isSuffixOf"  prop_isSuffixOfPL
    , testProperty "isInfixOf"   prop_isInfixOfPL
    , testProperty "stripPrefix" prop_stripPrefixPL
    , testProperty "stripSuffix" prop_stripSuffixPL
    , testProperty "length"      prop_lengthPL
    , testProperty "map"         prop_mapPL
    , testProperty "null"        prop_nullPL
    , testProperty "reverse"     prop_reversePL
    , testProperty "snoc"        prop_snocPL
    , testProperty "transpose"   prop_transposePL
    , testProperty "replicate"   prop_replicatePL
    , testProperty "take"        prop_takePL
    , testProperty "drop"        prop_dropPL
    , testProperty "splitAt"     prop_splitAtPL
    , testProperty "takeWhile"   prop_takeWhilePL
    , testProperty "dropWhile"   prop_dropWhilePL
    , testProperty "break"       prop_breakPL
    , testProperty "span"        prop_spanPL
    , testProperty "group"       prop_groupPL
    , testProperty "groupBy"     prop_groupByPL
    , testProperty "inits"       prop_initsPL
    , testProperty "tails"       prop_tailsPL
    , testProperty "elem"        prop_elemPL
    , testProperty "notElem"     prop_notElemPL
    , testProperty "lines"       prop_linesPL
    , testProperty "elemIndex"   prop_elemIndexPL
    , testProperty "elemIndex"   prop_elemIndexCL
    , testProperty "elemIndices" prop_elemIndicesPL
    , testProperty "concatMap"   prop_concatMapPL
    , testProperty "IsString"    prop_isstring
    , testProperty "IsString LC" prop_isstring_lc
    ]

------------------------------------------------------------------------
-- extra ByteString properties

bb_tests =
    [ testProperty "bijection"      prop_bijectionBB
    , testProperty "bijection'"     prop_bijectionBB'
    , testProperty "pack/unpack"    prop_packunpackBB
    , testProperty "unpack/pack"    prop_packunpackBB'
    , testProperty "eq 1"           prop_eq1BB
    , testProperty "eq 2"           prop_eq2BB
    , testProperty "eq 3"           prop_eq3BB
    , testProperty "compare 1"      prop_compare1BB
    , testProperty "compare 2"      prop_compare2BB
    , testProperty "compare 3"      prop_compare3BB
    , testProperty "compare 4"      prop_compare4BB
    , testProperty "compare 5"      prop_compare5BB
    , testProperty "compare 6"      prop_compare6BB
    , testProperty "compare 7"      prop_compare7BB
    , testProperty "compare 7"      prop_compare7LL
    , testProperty "compare 8"      prop_compare8BB
    , testProperty "empty 1"        prop_nil1BB
    , testProperty "empty 2"        prop_nil2BB
    , testProperty "empty 1 monoid" prop_nil1LL_monoid
    , testProperty "empty 2 monoid" prop_nil2LL_monoid
    , testProperty "empty 1 monoid" prop_nil1BB_monoid
    , testProperty "empty 2 monoid" prop_nil2BB_monoid

    , testProperty "null"           prop_nullBB
    , testProperty "length 1"       prop_lengthBB
    , testProperty "length 2"       prop_lengthSBB
    , testProperty "cons 1"         prop_consBB
    , testProperty "cons 2"         prop_cons1BB
    , testProperty "cons 3"         prop_cons2BB
    , testProperty "cons 4"         prop_cons3BB
    , testProperty "cons 5"         prop_cons4BB
    , testProperty "snoc"           prop_snoc1BB
    , testProperty "head 1"         prop_head1BB
    , testProperty "head 2"         prop_head2BB
    , testProperty "head 3"         prop_head3BB
    , testProperty "tail"           prop_tailBB
    , testProperty "tail 1"         prop_tail1BB
    , testProperty "last"           prop_lastBB
    , testProperty "last 1"         prop_last1BB
    , testProperty "init"           prop_initBB
    , testProperty "init 1"         prop_init1BB
    , testProperty "append 1"       prop_append1BB
    , testProperty "append 2"       prop_append2BB
    , testProperty "append 3"       prop_append3BB
    , testProperty "mappend 1"      prop_append1BB_monoid
    , testProperty "mappend 2"      prop_append2BB_monoid
    , testProperty "mappend 3"      prop_append3BB_monoid

    , testProperty "map 1"          prop_map1BB
    , testProperty "map 2"          prop_map2BB
    , testProperty "map 3"          prop_map3BB
    , testProperty "filter1"        prop_filter1BB
    , testProperty "filter2"        prop_filter2BB
    , testProperty "map fusion"     prop_mapfusionBB
    , testProperty "filter fusion"  prop_filterfusionBB
    , testProperty "reverse 1"      prop_reverse1BB
    , testProperty "reverse 2"      prop_reverse2BB
    , testProperty "reverse 3"      prop_reverse3BB
    , testProperty "foldl 1"        prop_foldl1BB
    , testProperty "foldl 2"        prop_foldl2BB
    , testProperty "foldr 1"        prop_foldr1BB
    , testProperty "foldr 2"        prop_foldr2BB
    , testProperty "foldl1 1"       prop_foldl1_1BB
    , testProperty "foldl1 2"       prop_foldl1_2BB
    , testProperty "foldl1 3"       prop_foldl1_3BB
    , testProperty "foldr1 1"       prop_foldr1_1BB
    , testProperty "foldr1 2"       prop_foldr1_2BB
    , testProperty "foldr1 3"       prop_foldr1_3BB
    , testProperty "scanl/foldl"    prop_scanlfoldlBB
    , testProperty "all"            prop_allBB
    , testProperty "any"            prop_anyBB
    , testProperty "take"           prop_takeBB
    , testProperty "drop"           prop_dropBB
    , testProperty "takeWhile_ne"   prop_takeWhileBB_ne
    , testProperty "takeWhile_eq"   prop_takeWhileBB_eq
    , testProperty "dropWhile_ne"   prop_dropWhileBB_ne
    , testProperty "dropWhile_eq"   prop_dropWhileBB_eq
    , testProperty "dropWhile_isSpace" prop_dropWhileCC_isSpace
    , testProperty "splitAt"        prop_splitAtBB
    , testProperty "span"           prop_spanBB
    , testProperty "break"          prop_breakBB
    , testProperty "elem"           prop_elemBB
    , testProperty "notElem"        prop_notElemBB

    , testProperty "concat 1"       prop_concat1BB
    , testProperty "concat 2"       prop_concat2BB
    , testProperty "concat 3"       prop_concatBB
    , testProperty "mconcat 1"      prop_concat1BB_monoid
    , testProperty "mconcat 2"      prop_concat2BB_monoid
    , testProperty "mconcat 3"      prop_concatBB_monoid

    , testProperty "mconcat 1"      prop_concat1LL_monoid
    , testProperty "mconcat 2"      prop_concat2LL_monoid
    , testProperty "mconcat 3"      prop_concatLL_monoid

    , testProperty "lines"          prop_linesBB
    , testProperty "unlines"        prop_unlinesBB
    , testProperty "unlines"        prop_unlinesLC
    , testProperty "lines_lazy1"    prop_lines_lazy1
    , testProperty "lines_lazy2"    prop_lines_lazy2
    , testProperty "words"          prop_wordsBB
    , testProperty "words"          prop_wordsLC
    , testProperty "unwords"        prop_unwordsBB
    , testProperty "group"          prop_groupBB
    , testProperty "groupBy 0"      prop_groupByBB
    , testProperty "groupBy 1"      prop_groupBy1CC
    , testProperty "groupBy 2"      prop_groupBy1BB
    , testProperty "groupBy 3"      prop_groupBy2CC
    , testProperty "join"           prop_joinBB
    , testProperty "elemIndex 1"    prop_elemIndex1BB
    , testProperty "elemIndex 2"    prop_elemIndex2BB
    , testProperty "findIndex"      prop_findIndexBB
    , testProperty "findIndexEnd"   prop_findIndexEndBB
    , testProperty "findIndexEnd"   prop_findIndexEndLL
    , testProperty "findIndexEnd"   prop_findIndexEndDD
    , testProperty "findIndicies"   prop_findIndiciesBB
    , testProperty "elemIndices"    prop_elemIndicesBB
    , testProperty "find"           prop_findBB
    , testProperty "find/findIndex" prop_find_findIndexBB
    , testProperty "sort 1"         prop_sort1BB
    , testProperty "sort 2"         prop_sort2BB
    , testProperty "sort 3"         prop_sort3BB
    , testProperty "sort 4"         prop_sort4BB
    , testProperty "sort 5"         prop_sort5BB
    , testProperty "intersperse"    prop_intersperseBB
    , testProperty "maximum"        prop_maximumBB
    , testProperty "minimum"        prop_minimumBB
    , testProperty "strip"          prop_strip
--  , testProperty "breakChar"      prop_breakCharBB
--  , testProperty "spanChar 1"     prop_spanCharBB
--  , testProperty "spanChar 2"     prop_spanChar_1BB
--  , testProperty "breakSpace"     prop_breakSpaceBB
--  , testProperty "dropSpace"      prop_dropSpaceBB
    , testProperty "spanEnd"        prop_spanEndBB
    , testProperty "breakEnd"       prop_breakEndBB
    , testProperty "breakEnd"       prop_breakEndCC
    , testProperty "elemIndexEnd"   prop_elemIndexEnd1BB
    , testProperty "elemIndexEnd"   prop_elemIndexEnd1CC
    , testProperty "elemIndexEnd"   prop_elemIndexEnd1LL
    , testProperty "elemIndexEnd"   prop_elemIndexEnd1DD
--  , testProperty "words'"         prop_wordsBB'
--  , testProperty "lines'"         prop_linesBB'
--  , testProperty "dropSpaceEnd"   prop_dropSpaceEndBB
    , testProperty "unfoldr"        prop_unfoldrBB
    , testProperty "prefix"         prop_prefixBB
    , testProperty "prefix"         prop_prefixLL
    , testProperty "suffix"         prop_suffixBB
    , testProperty "suffix"         prop_suffixLL
    , testProperty "stripPrefix"    prop_stripPrefixBB
    , testProperty "stripPrefix"    prop_stripPrefixLL
    , testProperty "stripSuffix"    prop_stripSuffixBB
    , testProperty "stripSuffix"    prop_stripSuffixLL
    , testProperty "copy"           prop_copyBB
    , testProperty "copy"           prop_copyLL
    , testProperty "inits"          prop_initsBB
    , testProperty "tails"          prop_tailsBB
    , testProperty "breakSubstring 1"prop_breakSubstringBB
    , testProperty "breakSubstring 3"prop_breakSubstring_isInfixOf

    , testProperty "replicate1"     prop_replicate1BB
    , testProperty "replicate2"     prop_replicate2BB
    , testProperty "replicate3"     prop_replicate3BB
    , testProperty "readInt"        prop_readintBB
    , testProperty "readInt 2"      prop_readint2BB
    , testProperty "readInteger"    prop_readintegerBB
    , testProperty "readInteger 2"  prop_readinteger2BB
    , testProperty "read"           prop_readLL
    , testProperty "read"           prop_readBB
    , testProperty "Lazy.readInt"   prop_readintLL
    , testProperty "Lazy.readInt"   prop_readintLL
    , testProperty "Lazy.readInteger" prop_readintegerLL

    , testProperty "readIntSafe"       prop_readIntSafe
    , testProperty "readIntUnsafe"     prop_readIntUnsafe
    , testProperty "readIntegerSafe"   prop_readIntegerSafe
    , testProperty "readIntegerUnsafe" prop_readIntegerUnsafe

    , testProperty "mconcat 1"      prop_append1LL_monoid
    , testProperty "mconcat 2"      prop_append2LL_monoid
    , testProperty "mconcat 3"      prop_append3LL_monoid
--  , testProperty "filterChar1"    prop_filterChar1BB
--  , testProperty "filterChar2"    prop_filterChar2BB
--  , testProperty "filterChar3"    prop_filterChar3BB
--  , testProperty "filterNotChar1" prop_filterNotChar1BB
--  , testProperty "filterNotChar2" prop_filterNotChar2BB
    , testProperty "tail"           prop_tailSBB
    , testProperty "index"          prop_indexBB
    , testProperty "unsafeIndex"    prop_unsafeIndexBB
--  , testProperty "map'"           prop_mapBB'
    , testProperty "filter"         prop_filterBB
    , testProperty "elem"           prop_elemSBB
    , testProperty "take"           prop_takeSBB
    , testProperty "drop"           prop_dropSBB
    , testProperty "splitAt"        prop_splitAtSBB
    , testProperty "foldl"          prop_foldlBB
    , testProperty "foldr"          prop_foldrBB
    , testProperty "takeWhile "     prop_takeWhileSBB
    , testProperty "dropWhile "     prop_dropWhileSBB
    , testProperty "span "          prop_spanSBB
    , testProperty "break "         prop_breakSBB
    , testProperty "breakspan"      prop_breakspan_1BB
    , testProperty "lines "         prop_linesSBB
    , testProperty "unlines "       prop_unlinesSBB
    , testProperty "words "         prop_wordsSBB
    , testProperty "unwords "       prop_unwordsSBB
    , testProperty "unwords "       prop_unwordsSLC
--     , testProperty "wordstokens"    prop_wordstokensBB
    , testProperty "splitWith_empty" prop_splitWithBB_empty
    , testProperty "splitWith"      prop_splitWithBB
    , testProperty "split_empty"    prop_splitBB_empty
    , testProperty "joinsplit"      prop_joinsplitBB
    , testProperty "intercalate"    prop_intercalatePL
--     , testProperty "lineIndices"    prop_lineIndices1BB
    , testProperty "count"          prop_countBB
--  , testProperty "linessplit"     prop_linessplit2BB
    , testProperty "splitsplitWith" prop_splitsplitWithBB
--  , testProperty "joinjoinpath"   prop_joinjoinpathBB
    , testProperty "zip"            prop_zipBB
    , testProperty "zip"            prop_zipLC
    , testProperty "zip1"           prop_zip1BB
    , testProperty "zipWithBB"        prop_zipWithBB
    , testProperty "zipWithCC"        prop_zipWithCC
    , testProperty "zipWithLC"        prop_zipWithLC
    , testProperty "packZipWithBB"    prop_packZipWithBB
    , testProperty "packZipWithLL"    prop_packZipWithLL
    , testProperty "packZipWithBC"    prop_packZipWithBC
    , testProperty "packZipWithLC"    prop_packZipWithLC
    , testProperty "unzip"          prop_unzipBB
    , testProperty "concatMap"      prop_concatMapBB
--  , testProperty "join/joinByte"  prop_join_spec
#if MIN_VERSION_base(4,9,0)
    , testProperty "stimes strict"  prop_stimesBB
    , testProperty "stimes lazy"    prop_stimesLL
#endif
    ]


------------------------------------------------------------------------
-- Extra lazy properties

ll_tests =
    [ testProperty "eq 1"               prop_eq1
    , testProperty "eq 2"               prop_eq2
    , testProperty "eq 3"               prop_eq3
    , testProperty "eq refl"            prop_eq_refl
    , testProperty "eq symm"            prop_eq_symm
    , testProperty "compare 1"          prop_compare1
    , testProperty "compare 2"          prop_compare2
    , testProperty "compare 3"          prop_compare3
    , testProperty "compare 4"          prop_compare4
    , testProperty "compare 5"          prop_compare5
    , testProperty "compare 6"          prop_compare6
    , testProperty "compare 7"          prop_compare7
    , testProperty "compare 8"          prop_compare8
    , testProperty "compare 9"          prop_compare9
    , testProperty "empty 1"            prop_empty1
    , testProperty "empty 2"            prop_empty2
    , testProperty "pack/unpack"        prop_packunpack
    , testProperty "unpack/pack"        prop_unpackpack
    , testProperty "null"               prop_null
    , testProperty "length 1"           prop_length1
    , testProperty "length 2"           prop_length2
    , testProperty "cons 1"             prop_cons1
    , testProperty "cons 2"             prop_cons2
    , testProperty "cons 3"             prop_cons3
    , testProperty "cons 4"             prop_cons4
    , testProperty "snoc"               prop_snoc1
    , testProperty "head/pack"          prop_head
    , testProperty "head/unpack"        prop_head1
    , testProperty "tail/pack"          prop_tail
    , testProperty "tail/unpack"        prop_tail1
    , testProperty "last"               prop_last
    , testProperty "init"               prop_init
    , testProperty "append 1"           prop_append1
    , testProperty "appendLazy"         prop_appendLazy
    , testProperty "append 2"           prop_append2
    , testProperty "append 3"           prop_append3
    , testProperty "map 1"              prop_map1
    , testProperty "map 2"              prop_map2
    , testProperty "map 3"              prop_map3
    , testProperty "filter 1"           prop_filter1
    , testProperty "filter 2"           prop_filter2
    , testProperty "reverse"            prop_reverse
    , testProperty "reverse1"           prop_reverse1
    , testProperty "reverse2"           prop_reverse2
    , testProperty "transpose"          prop_transpose
    , testProperty "foldl"              prop_foldl
    , testProperty "foldl/reverse"      prop_foldl_1
    , testProperty "foldr"              prop_foldr
    , testProperty "foldr/id"           prop_foldr_1
    , testProperty "foldl1/foldl"       prop_foldl1_1
    , testProperty "foldl1/head"        prop_foldl1_2
    , testProperty "foldl1/tail"        prop_foldl1_3
    , testProperty "foldr1/foldr"       prop_foldr1_1
    , testProperty "foldr1/last"        prop_foldr1_2
    , testProperty "foldr1/head"        prop_foldr1_3
    , testProperty "concat 1"           prop_concat1
    , testProperty "concat 2"           prop_concat2
    , testProperty "concat/pack"        prop_concat3
    , testProperty "any"                prop_any
    , testProperty "all"                prop_all
    , testProperty "maximum"            prop_maximum
    , testProperty "minimum"            prop_minimum
    , testProperty "compareLength 1"    prop_compareLength1
    , testProperty "compareLength 2"    prop_compareLength2
    , testProperty "compareLength 3"    prop_compareLength3
    , testProperty "compareLength 4"    prop_compareLength4
    , testProperty "compareLength 5"    prop_compareLength5
    , testProperty "replicate 1"        prop_replicate1
    , testProperty "replicate 2"        prop_replicate2
    , testProperty "take"               prop_take1
    , testProperty "takeEnd"            prop_takeEnd
    , testProperty "drop"               prop_drop1
    , testProperty "dropEnd"            prop_dropEnd
    , testProperty "splitAt"            prop_drop1
    , testProperty "takeWhile"          prop_takeWhile
    , testProperty "dropWhile"          prop_dropWhile
    , testProperty "takeWhileEnd"       prop_takeWhileEnd
    , testProperty "dropWhileEnd"       prop_dropWhileEnd
    , testProperty "break"              prop_break
    , testProperty "span"               prop_span
    , testProperty "splitAt"            prop_splitAt
    , testProperty "break/span"         prop_breakspan
    , testProperty "split"              prop_split
    , testProperty "splitWith_empty"    prop_splitWith_empty
    , testProperty "splitWith"          prop_splitWith
    , testProperty "splitWith_empty"    prop_splitWith_D_empty
    , testProperty "splitWith"          prop_splitWith_D
    , testProperty "splitWith_empty"    prop_splitWith_C_empty
    , testProperty "splitWith"          prop_splitWith_C
    , testProperty "split_empty"        prop_split_empty
    , testProperty "join.split/id"      prop_joinsplit
--  , testProperty "join/joinByte"      prop_joinjoinByte
    , testProperty "group"              prop_group
    , testProperty "groupBy"            prop_groupBy
    , testProperty "groupBy"            prop_groupBy_LC
    , testProperty "index"              prop_index
    , testProperty "index"              prop_index_D
    , testProperty "index"              prop_index_C
    , testProperty "indexMaybe"         prop_indexMaybe_Just_P
    , testProperty "indexMaybe"         prop_indexMaybe_Just_L
    , testProperty "indexMaybe"         prop_indexMaybe_Nothing_P
    , testProperty "indexMaybe"         prop_indexMaybe_Nothing_L
    , testProperty "elemIndex"          prop_elemIndex
    , testProperty "elemIndices"        prop_elemIndices
    , testProperty "count/elemIndices"  prop_count
    , testProperty "findIndex"          prop_findIndex
    , testProperty "findIndexEnd"       prop_findIndexEnd
    , testProperty "findIndices"        prop_findIndicies
    , testProperty "find"               prop_find
    , testProperty "find/findIndex"     prop_find_findIndex
    , testProperty "elem"               prop_elem
    , testProperty "notElem"            prop_notElem
    , testProperty "elem/notElem"       prop_elem_notelem
--  , testProperty "filterByte 1"       prop_filterByte
--  , testProperty "filterByte 2"       prop_filterByte2
--  , testProperty "filterNotByte 1"    prop_filterNotByte
--  , testProperty "filterNotByte 2"    prop_filterNotByte2
    , testProperty "isPrefixOf"         prop_isPrefixOf
    , testProperty "isSuffixOf"         prop_isSuffixOf
    , testProperty "stripPrefix"        prop_stripPrefix
    , testProperty "stripSuffix"        prop_stripSuffix
    , testProperty "concatMap"          prop_concatMap
    , testProperty "isSpace"            prop_isSpaceWord8
    ]

findIndexEnd :: (a -> Bool) -> [a] -> Maybe Int
findIndexEnd p = go . findIndices p
  where
    go [] = Nothing
    go (k:[]) = Just k
    go (k:ks) = go ks

elemIndexEnd :: Eq a => a -> [a] -> Maybe Int
elemIndexEnd = findIndexEnd . (==)

removeFile :: String -> IO ()
removeFile fn = void $ withCString fn c_unlink
