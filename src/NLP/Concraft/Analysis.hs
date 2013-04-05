-- | Morphosyntactic analysis utilities.
--
-- See `reAnaSent` function for a description of how reanalsis is performed.
-- At some point it would be nice to change the entire process so that
-- sentence-level segmentation is also taken from the reanalysed data.


module NLP.Concraft.Analysis
(
-- * Analysis
  Analyse
-- * Reanalysis
, reAnaSent
, reAnaPar
) where


import qualified Data.Text.Lazy as L

import           NLP.Concraft.Morphosyntax
import qualified Data.Tagset.Positional as P
import qualified NLP.Concraft.Morphosyntax.Align as A


---------------------
-- Analysis
---------------------


-- | An analyser performs word-level segmentation and morphological analysis.
type Analyse w t = L.Text -> Sent w t


---------------------
-- Reanalysis
---------------------


-- | Reanalyse sentence.
--
-- From the reference sentence the function takes:
--
--   * Word-level segmentation
--
--   * Chosen interpretations (tags)
--
-- From the reanalysed sentence the function takes:
--
--   * Potential interpretations
--
reAnaSent :: Word w => P.Tagset -> Analyse w P.Tag
          -> SentO w P.Tag -> Sent w P.Tag
reAnaSent tagset ana sent =
    A.sync tagset gold reana
  where
    gold  = segs sent
    reana = ana (orig sent)


-- | Reanalyse paragraph.
reAnaPar :: Word w => P.Tagset -> Analyse w P.Tag
         -> [SentO w P.Tag] -> [Sent w P.Tag]
reAnaPar tagset ana = map (reAnaSent tagset ana)


---------------------
-- Junk
---------------------


-- -- | Reanalyse paragraph.
-- reanalyse :: Word w => P.Tagset -> Analyse w P.Tag
--           -> [SentO w P.Tag] -> [Sent w P.Tag]
-- reanalyse tagset ana xs = chunk
--     -- We have to take sentence lengths from the reference corpus because
--     -- token-level segmentation is also taken from the reference corpus
--     -- (in case of inconsistencies between the two corpora).
--     (map length gold)
--     (A.sync tagset (concat gold) (concat reana))
--   where
--     gold  = map segs xs
--     reana = ana . L.concat $ map orig xs
--
--
-- -- | Divide the list into a list of chunks given the list of
-- -- lengths of individual chunks.
-- chunk :: [Int] -> [a] -> [[a]]
-- chunk (n:ns) xs = 
--     let (first, rest) = splitAt n xs 
--     in  first : chunk ns rest
-- chunk [] [] = []
-- chunk [] _  = error "chunk: absurd"
