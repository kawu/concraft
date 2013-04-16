-- | Accuracy statistics.


module NLP.Concraft.Morphosyntax.Accuracy
(
-- * Stats
  Stats (..)
, accuracy

-- * Accuracy
, weakLB
, weakUB
, strongLB
, strongUB
) where 


import           Data.List (foldl')
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Tagset.Positional as P
import           NLP.Concraft.Morphosyntax
import           NLP.Concraft.Morphosyntax.Align


-- | Statistics.
data Stats = Stats
    { good :: Int   -- ^ Number of correct tags
    , gold :: Int } -- ^ Number of segments in gold corpus


-- | Add stats,
(.+.) :: Stats -> Stats -> Stats
Stats x y .+. Stats x' y' = Stats (x + x') (y + y')


-- | Accuracy given stats.
accuracy :: Stats -> Double
accuracy s
    = fromIntegral (good s)
    / fromIntegral (gold s)


-- | Accuracy weak lower bound.
weakLB :: Word w => P.Tagset -> [Seg w P.Tag] -> [Seg w P.Tag] -> Stats
weakLB tagset ref other =
    foldl' (.+.) (Stats 0 0) . map (uncurry stats) $ align ref other
  where
    stats [x] [y]
        | S.null (xTags `S.intersection` yTags) = Stats 0 1
        | otherwise = Stats 1 1
      where
        xTags = choice tagset x
        yTags = choice tagset y
    stats xs _ = Stats 0 (length xs)


-- | Accuracy strong lower bound.
strongLB :: Word w => P.Tagset -> [Seg w P.Tag] -> [Seg w P.Tag] -> Stats
strongLB tagset ref other =
    foldl' (.+.) (Stats 0 0) . map (uncurry stats) $ align ref other
  where
    stats [x] [y]
        | xTags == yTags = Stats 1 1
        | otherwise = Stats 0 1
      where
        xTags = choice tagset x
        yTags = choice tagset y
    stats xs _ = Stats 0 (length xs)


-- | Accuracy weak upper bound.
weakUB :: Word w => P.Tagset -> [Seg w P.Tag] -> [Seg w P.Tag] -> Stats
weakUB tagset ref other =
    foldl' (.+.) (Stats 0 0) . map (uncurry stats) $ align ref other
  where
    stats [x] [y]
        | S.null (xTags `S.intersection` yTags) = Stats 0 1
        | otherwise = Stats 1 1
      where
        xTags = choice tagset x
        yTags = choice tagset y
    stats xs _ = Stats (length xs) (length xs)


-- | Accuracy strong upper bound.
strongUB :: Word w => P.Tagset -> [Seg w P.Tag] -> [Seg w P.Tag] -> Stats
strongUB tagset ref other =
    foldl' (.+.) (Stats 0 0) . map (uncurry stats) $ align ref other
  where
    stats [x] [y]
        | xTags == yTags = Stats 1 1
        | otherwise = Stats 0 1
      where
        xTags = choice tagset x
        yTags = choice tagset y
    stats xs _ = Stats (length xs) (length xs)
    

-- | All tags are expanded here.
choice :: P.Tagset -> Seg w P.Tag -> S.Set P.Tag
choice tagset = S.fromList . concatMap (P.expand tagset) . positive


-- | Positive tags.
positive :: Seg w t -> [t]
positive seg =
    let xs = M.toList . unWMap . tags
    in  [x | (x, v) <- xs seg, v > 0]
