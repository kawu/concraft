{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Simple format for morphosyntax representation which
-- assumes that all tags have a textual representation
-- with no spaces inside and that one of the tags indicates
-- unknown words.

module NLP.Concraft.Format.Plain
( plainFormat
-- -- * Types
-- , Space (..)
-- , Token (..)
-- , Interp (..)
-- 
-- -- * Parsing
-- , readPlain
-- , parsePlain
-- , parseSent
-- 
-- -- * Showing
-- , writePlain
-- , showPlain
-- , showSent
-- , showWord
) where

import Data.Monoid (Monoid, mappend, mconcat)
import Data.Maybe (catMaybes)
import Data.List (groupBy)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as L

import qualified NLP.Concraft.Morphosyntax as Mx
import qualified NLP.Concraft.Format as F

-- | No space, space or newline.
data Space
    = None
    | Space
    | NewLine
    deriving (Show, Eq, Ord)

-- | A token.
data Token = Token
    { orth      :: T.Text
    , space     :: Space
    , known     :: Bool
    -- | Interpretations of the token, each interpretation annotated
    -- with a /disamb/ Boolean value (if 'True', the interpretation
    -- is correct within the context).
    , interps   :: M.Map Interp Bool }
    deriving (Show, Eq, Ord)
    
data Interp = Interp
    { _base     :: T.Text
    , tag       :: T.Text }
    deriving (Show, Eq, Ord)

-- | Create document handler given value of the /ignore/ tag.
plainFormat :: T.Text -> F.Format [] [Token] Token
plainFormat ign = F.Format (parsePlain ign) (showPlain ign) sentHandler

-- | Sentence handler.
sentHandler :: F.Sent [Token] Token
sentHandler = F.Sent id (\xs _ -> xs) wordHandler

-- | Word handler.
wordHandler :: F.Word Token
wordHandler = F.Word extract select

-- | Extract information relevant for tagging.
extract :: Token -> Mx.Word T.Text
extract tok = Mx.Word
    { Mx.orth   = orth tok
    , Mx.tags   = Mx.mkProb
        [ (tag x, if disamb then 1 else 0)
        | (x, disamb) <- M.toList (interps tok) ]
    , Mx.oov    = not (known tok) }

-- | Select interpretations.
select :: Mx.Prob T.Text -> Token -> Token
select pr tok =
    let xs = M.fromList
            [ ( Interp "None" tag
              , if x > 0 then True else False )
            | (tag, x) <- M.toList (Mx.unProb pr) ]
    in  tok { interps = xs }

-- -- | Add new interpretations with given /disamb/ annotation.
-- addInterps :: Bool -> Token -> [Interp] -> Token
-- addInterps dmb tok xs =
--     let newIps = M.fromList [(x, dmb) | x <- xs]
--     in  tok { interps = M.unionWith max newIps (interps tok) }
-- 
-- -- | Add new interpretations with "None" base form and given
-- -- /disamb/ annotation.
-- addNones :: Bool -> Token -> [T.Text] -> Token
-- addNones dmb tok = addInterps dmb tok . map (Interp "None")
-- 
-- -- | Select interpretations with tags belonging to the given set.
-- -- Interpretations selected in the process (and only those selected)
-- -- will be marked with 'True' /disamb/ values.
-- select :: Token -> S.Set T.Text -> Token
-- select tok choice =
--     tok { interps = (M.fromList . map mark . M.keys) (interps tok) }
--   where
--     mark ip 
--         | tag ip `S.member` choice  = (ip, True) 
--         | otherwise                 = (ip, False)

-- readPlain :: T.Text -> FilePath -> IO [[Token]]
-- readPlain ign = fmap (parsePlain ign) . L.readFile

parsePlain :: T.Text -> L.Text -> [[Token]]
parsePlain ign = map (parseSent ign) . init . L.splitOn "\n\n"

parseSent :: T.Text -> L.Text -> [Token]
parseSent ign
    = map (parseWord ignL)
    . groupBy (\_ x -> cond x)
    . L.lines
  where
    cond = ("\t" `L.isPrefixOf`)
    ignL = L.fromStrict ign

parseWord :: L.Text -> [L.Text] -> Token
parseWord ign xs =
    (Token _orth _space _known _interps)
  where
    (_orth, _space) = parseHeader (head xs)
    ys          = map (parseInterp ign) (tail xs)
    _known      = not (Nothing `elem` ys)
    _interps    = M.fromListWith max (catMaybes ys)

parseInterp :: L.Text -> L.Text -> Maybe (Interp, Bool)
parseInterp ign =
    doIt . tail . L.splitOn "\t"
  where
    doIt [form, tag]
        | tag == ign    = Nothing
        | otherwise     = Just $
            (mkInterp form tag, False)
    doIt [form, tag, "disamb"] = Just $
        (mkInterp form tag, True)
    doIt xs = error $ "parseInterp: " ++ show xs
    mkInterp form tag = Interp (L.toStrict form) (L.toStrict tag)

parseHeader :: L.Text -> (T.Text, Space)
parseHeader xs =
    let [_orth, space] = L.splitOn "\t" xs
    in  (L.toStrict _orth, parseSpace space)

parseSpace :: L.Text -> Space
parseSpace "none"    = None
parseSpace "space"   = Space
parseSpace "newline" = NewLine
parseSpace "newlines" = NewLine -- TODO: Remove this temporary fix
parseSpace xs        = error ("parseSpace: " ++ L.unpack xs)

-- | Printing.

-- | An infix synonym for 'mappend'.
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}

-- writePlain :: T.Text -> FilePath -> [[Token]] -> IO ()
-- writePlain ign path = L.writeFile path . showPlain ign 

showPlain :: T.Text -> [[Token]] -> L.Text
showPlain ign =
    L.toLazyText . mconcat  . map (\xs -> buildSent ign xs <> "\n")

-- showSent :: T.Text -> [Token] -> L.Text
-- showSent ign = L.toLazyText . buildSent ign
-- 
-- showWord :: T.Text -> Token -> L.Text
-- showWord ign = L.toLazyText . buildWord ign

buildSent :: T.Text -> [Token] -> L.Builder
buildSent ign = mconcat . map (buildWord ign)

buildWord :: T.Text -> Token -> L.Builder
buildWord ign tok
    =  L.fromText (orth tok) <> "\t"
    <> buildSpace (space tok) <> "\n"
    <> buildKnown ign (known tok)
    <> buildInterps (M.toList $ interps tok)

buildInterps :: [(Interp, Bool)] -> L.Builder
buildInterps interps = mconcat
    [ "\t" <> L.fromText _base <>
      "\t" <> L.fromText _tag <>
      if dmb
        then "\tdisamb\n"
        else "\n"
    | (Interp _base _tag, dmb) <- interps ]

buildSpace :: Space -> L.Builder
buildSpace None     = "none"
buildSpace Space    = "space"
buildSpace NewLine  = "newline"

buildKnown :: T.Text -> Bool -> L.Builder
buildKnown _ True       = ""
buildKnown ign False    = "\tNone\t" <> L.fromText ign <> "\n"
