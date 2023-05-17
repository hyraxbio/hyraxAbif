{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hyax.Abif.Fasta
Description : Read a FASTA file
Copyright   : (c) HyraxBio, 2018
License     : BSD3
Maintainer  : andre@hyraxbio.co.za, andre@andrevdm.com
Stability   : beta

Functionality for reading FASTA files
-}
module Hyrax.Abif.Fasta
    ( Fasta (..)
    , parseFasta
    ) where

import           Verset hiding (lines)
import qualified Data.Text as Txt

-- | FASTA data
data Fasta = Fasta { fastaName :: !Text -- ^ Name
                   , fastaRead :: !Text -- ^ Data
                   } deriving (Show, Eq)


-- | Parse the data for a single FASTA into a list of 'Fasta' values.
-- Single and multi-line FASTAs are supported.
-- Used by "Hyrax.Abif.Generate" to read weighted-FASTAs
parseFasta :: Text -> Either Text [Fasta]
parseFasta s =
  reverse <$> go (Txt.lines s) Nothing "" []

  where
    go :: [Text] -> Maybe Text -> Text -> [Fasta] -> Either Text [Fasta]
    go (line:lines) (Just name) read acc =
      if Txt.take 1 line /= ">"
      then go lines (Just name) (read <> line) acc
      else go lines (Just $ Txt.drop 1 line) "" (Fasta (Txt.strip name) read : acc)
    go (line:lines) Nothing _read acc =
      if Txt.take 1 line == ">"
      then go lines (Just $ Txt.strip . Txt.drop 1 $ line) "" acc
      else Left "Expecting name"
    go [] Nothing _ acc =
      Right acc
    go [] (Just _name) "" _acc =
      Left "Expecting read"
    go [] (Just name) read acc =
      Right $ Fasta (Txt.strip name) read : acc
