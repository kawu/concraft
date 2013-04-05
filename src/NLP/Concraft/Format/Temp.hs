{-# LANGUAGE OverloadedStrings #-}

module NLP.Concraft.Format.Temp
( encodePar
, decodePar
, writePar
, readPar
) where

import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Text as T
import           Data.Aeson

import           NLP.Concraft.Morphosyntax

encodePar :: ToJSON w => [Sent w T.Text] -> BC.ByteString
encodePar = BC.unlines . map encode

decodePar :: FromJSON w => BC.ByteString -> [Sent w T.Text]
decodePar = 
    let getRight (Right x) = x
        getRight (Left e)  = error $ "error in decodePar: " ++ e
    in  map (getRight . eitherDecode') . BC.lines

writePar :: ToJSON w => FilePath -> [Sent w T.Text] -> IO ()
writePar path = BC.writeFile path . encodePar

readPar :: FromJSON w => FilePath -> IO [Sent w T.Text]
readPar = fmap decodePar . BC.readFile
