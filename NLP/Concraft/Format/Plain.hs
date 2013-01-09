{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Simple format for morphosyntax representation which
-- assumes that all tags have a textual representation
-- with no spaces inside and that one of the tags indicates
-- unknown words.

module NLP.Concraft.Format.Plain
( plainFormat
) where

import Control.Arrow (first)
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
    { base  :: Maybe T.Text
    , tag   :: F.Tag }
    deriving (Show, Eq, Ord)

noneBase :: T.Text
noneBase = "None"

-- | Create document handler given value of the /ignore/ tag.
plainFormat :: F.Tag -> F.Doc [] [Token] Token
plainFormat ign = F.Doc (parsePlain ign) (showPlain ign) sentHandler

-- | Sentence handler.
sentHandler :: F.Sent [Token] Token
sentHandler = F.Sent id (\xs _ -> xs) wordHandler

-- | Word handler.
wordHandler :: F.Word Token
wordHandler = F.Word extract select

-- | Extract information relevant for tagging.
extract :: Token -> Mx.Word F.Tag
extract tok = Mx.Word
    { Mx.orth       = orth tok
    , Mx.tagWMap    = Mx.mkWMap
        [ (tag x, if disamb then 1 else 0)
        | (x, disamb) <- M.toList (interps tok) ]
    , Mx.oov        = not (known tok) }

-- | Select interpretations.
select :: Mx.WMap F.Tag -> Token -> Token
select wMap tok =
    tok { interps = newInterps }
  where
    wSet = M.fromList . map (first tag) . M.toList . interps
    asDmb x = if x > 0
        then True
        else False
    newInterps = M.fromList $
        [ case M.lookup (tag interp) (Mx.unWMap wMap) of
            Just x  -> (interp, asDmb x)
            Nothing -> (interp, False)
        | interp <- M.keys (interps tok) ]
            ++ catMaybes
        [ if tag `M.member` wSet tok
            then Nothing
            else Just (Interp Nothing tag, asDmb x)
        | (tag, x) <- M.toList (Mx.unWMap wMap) ]

parsePlain :: F.Tag -> L.Text -> [[Token]]
parsePlain ign = map (parseSent ign) . init . L.splitOn "\n\n"

parseSent :: F.Tag -> L.Text -> [Token]
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
    mkInterp form tag
        | formS == noneBase = Interp Nothing tagS
        | otherwise         = Interp (Just formS) tagS
      where
        formS   = L.toStrict form
        tagS    = L.toStrict tag

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

showPlain :: F.Tag -> [[Token]] -> L.Text
showPlain ign =
    L.toLazyText . mconcat  . map (\xs -> buildSent ign xs <> "\n")

buildSent :: F.Tag -> [Token] -> L.Builder
buildSent ign = mconcat . map (buildWord ign)

buildWord :: F.Tag -> Token -> L.Builder
buildWord ign tok
    =  L.fromText (orth tok) <> "\t"
    <> buildSpace (space tok) <> "\n"
    <> buildKnown ign (known tok)
    <> buildInterps (M.toList $ interps tok)

buildInterps :: [(Interp, Bool)] -> L.Builder
buildInterps interps = mconcat
    [ "\t" <> buildBase interp <>
      "\t" <> buildTag  interp <>
      if dmb
        then "\tdisamb\n"
        else "\n"
    | (interp, dmb) <- interps ]
  where
    buildTag    = L.fromText . tag
    buildBase x = case base x of
        Just b  -> L.fromText b
        Nothing -> L.fromText noneBase

buildSpace :: Space -> L.Builder
buildSpace None     = "none"
buildSpace Space    = "space"
buildSpace NewLine  = "newline"

buildKnown :: F.Tag -> Bool -> L.Builder
buildKnown _   True     = ""
buildKnown ign False    =  "\t" <> L.fromText noneBase
                        <> "\t" <> L.fromText ign <> "\n"
