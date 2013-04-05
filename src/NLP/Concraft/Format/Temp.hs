{-# LANGUAGE OverloadedStrings #-}

module NLP.Concraft.Format.Temp
( encodePar
, decodePar
, writePar
, readPar
) where

import           Data.Maybe (catMaybes)
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Text as T
import           Data.Aeson
import           Data.Aeson.Encode.Pretty

import           NLP.Concraft.Morphosyntax

encodePar :: ToJSON w => [Sent w T.Text] -> BC.ByteString
encodePar = BC.intercalate "\n" . map encodePretty

-- | TODO: Incorrect!
decodePar :: FromJSON w => BC.ByteString -> [Sent w T.Text]
decodePar = catMaybes . map decode' . BC.split '\n'

writePar :: ToJSON w => FilePath -> [Sent w T.Text] -> IO ()
writePar path = BC.writeFile path . encodePar

readPar :: FromJSON w => FilePath -> IO [Sent w T.Text]
readPar = fmap decodePar . BC.readFile
