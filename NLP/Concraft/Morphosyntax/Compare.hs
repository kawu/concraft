module NLP.Concraft.Morphosyntax.Compare
( WC
, Stats (..)
, align
, weakLB
, weakUB
, strongLB
, strongUB
, accuracy
) where

import Data.Char (isSpace)
import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map as M

import qualified NLP.Concraft.Morphosyntax.Base as Base

type WC t = (Base.Word t, Base.Choice t)

word :: WC t -> Base.Word t
word = fst

choice :: WC t -> S.Set (Base.Interp t)
choice = M.keysSet . snd

orth :: WC t -> T.Text
orth = Base.orth . word

align :: [WC t] -> [WC t] -> [([WC t], [WC t])]
align [] [] = []
align [] _  = error "align: null xs, not null ys"
align _  [] = error "align: not null xs, null ys"
align xs ys =
    let (x, y) = match xs ys
    in  (x, y) : align (drop (length x) xs) (drop (length y) ys)
    
match :: [WC t] -> [WC t] -> ([WC t], [WC t])
match xs' ys' =
    doIt 0 xs' 0 ys'
  where
    doIt i (x:xs) j (y:ys)
        | n == m    = ([x], [y])
        | n <  m    = x <: doIt n xs j (y:ys)
        | otherwise = y >: doIt i (x:xs) m ys
      where
        n = i + size x
        m = j + size y
    doIt _ _ _ _ = error "match: null input list"
    size w = T.length . T.filter (not.isSpace) $ orth w
    x <: (xs, ys) = (x:xs, ys)
    y >: (xs, ys) = (xs, y:ys)

data Stats = Stats
    { good :: !Int   -- ^ Number of correct tags
    , gold :: !Int } -- ^ Number of segments in gold corpus

(.+.) :: Stats -> Stats -> Stats
Stats x y .+. Stats x' y' = Stats (x + x') (y + y')

accuracy :: Stats -> Double
accuracy s
    = fromIntegral (good s)
    / fromIntegral (gold s)

weakLB :: Ord t => [WC t] -> [WC t] -> Stats
weakLB xs ys =
    foldl' (.+.) (Stats 0 0) . map (uncurry stats) $ align xs ys
  where
    stats [x] [y]
        | S.null (choice x `S.intersection` choice y)   = Stats 0 1
        | otherwise                                     = Stats 1 1
    stats xs' _ = Stats 0 (length xs')

strongLB :: Eq t => [WC t] -> [WC t] -> Stats
strongLB xs ys =
    foldl' (.+.) (Stats 0 0) . map (uncurry stats) $ align xs ys
  where
    stats [x] [y]
        | choice x == choice y  = Stats 1 1
        | otherwise             = Stats 0 1
    stats xs' _ = Stats 0 (length xs')

weakUB :: Ord t => [WC t] -> [WC t] -> Stats
weakUB xs ys =
    foldl' (.+.) (Stats 0 0) . map (uncurry stats) $ align xs ys
  where
    stats [x] [y]
        | S.null (choice x `S.intersection` choice y)   = Stats 0 1
        | otherwise                                     = Stats 1 1
    stats xs' _ = Stats (length xs') (length xs')

strongUB :: Eq t => [WC t] -> [WC t] -> Stats
strongUB xs ys =
    foldl' (.+.) (Stats 0 0) . map (uncurry stats) $ align xs ys
  where
    stats [x] [y]
        | choice x == choice y  = Stats 1 1
        | otherwise             = Stats 0 1
    stats xs' _ = Stats (length xs') (length xs')
