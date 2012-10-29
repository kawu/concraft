{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Simple format for morphosyntax representation which
-- assumes that all tags have a textual representation
-- with no spaces inside and that one of the tags indicates
-- unknown words.

module NLP.Concraft.Plain
(
-- * Types
  Space (..)
, Token (..)
, Interp (..)

-- * Interface
, fromTok
, choose

-- * Parsing
, readPlain
, parsePlain
, parseSent

-- * Showing
, writePlain
, showPlain
, showSent
, showWord
) where

import Data.Monoid (Monoid, mappend, mconcat)
import Data.Maybe (catMaybes)
import Data.List (groupBy)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import qualified Data.Text.Lazy.Builder as L

import qualified NLP.Concraft.Morphosyntax as Mx

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
    -- | Interpretations with disambiguation info.
    , interps   :: M.Map Interp Bool }
    deriving (Show, Eq, Ord)
    
data Interp = Interp
    { base      :: T.Text
    , tag       :: T.Text }
    deriving (Show, Eq, Ord)

-- | Extract information relevant for tagging.
fromTok :: Token -> (Mx.Word T.Text, Mx.Choice T.Text)
fromTok tok =
    (word, choice)
  where
    word = Mx.Word
        { Mx.orth   = orth tok
        , Mx.tags   = if known tok
            then S.fromList . map tag . M.keys $ interps tok
            else S.empty }
    choice = M.fromListWith (Mx.<+>)
        [ (tag x, Mx.mkPositive 1)
        | (x, True) <- M.toList (interps tok) ]

-- | Mark all interpretations with tag component beeing a member of
-- the given choice set with disamb annotations.
choose :: Token -> S.Set T.Text -> Token
choose tok choice =
    tok { interps = (M.fromList . map mark . M.keys) (interps tok) }
  where
    mark ip 
        | tag ip `S.member` choice  = (ip, True) 
        | otherwise                 = (ip, False)

readPlain :: T.Text -> FilePath -> IO [[Token]]
readPlain ign = fmap (parsePlain ign) . L.readFile

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
    _interps    = M.fromList (catMaybes ys)

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
parseSpace "newlines" = NewLine -- ^ TODO: Remove this temporary fix
parseSpace xs        = error ("parseSpace: " ++ L.unpack xs)

-- | Printing.

-- | An infix synonym for 'mappend'.
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}

writePlain :: T.Text -> FilePath -> [[Token]] -> IO ()
writePlain ign path = L.writeFile path . showPlain ign 

showPlain :: T.Text -> [[Token]] -> L.Text
showPlain ign =
    L.toLazyText . mconcat  . map (\xs -> buildSent ign xs <> "\n")

showSent :: T.Text -> [Token] -> L.Text
showSent ign = L.toLazyText . buildSent ign

showWord :: T.Text -> Token -> L.Text
showWord ign = L.toLazyText . buildWord ign

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
