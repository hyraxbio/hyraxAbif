{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Hyrax.Abi.Fasta
    ( Fasta (..)
    , parseFasta
    ) where

import           Protolude
import qualified Data.Text as Txt

data Fasta = Fasta { _fastaName :: !Text
                   , _fastaRead :: !Text
                   } deriving (Show, Eq)


parseFasta :: Text -> Either Text [Fasta]
parseFasta s =
  reverse <$> go (Txt.lines s) Nothing "" []

  where
    go :: [Text] -> Maybe Text -> Text -> [Fasta] -> Either Text [Fasta]
    go (line:lines) (Just name) read acc =
      if Txt.take 1 line /= ">"
      then go lines (Just name) (read <> line) acc
      else go lines (Just $ Txt.drop 1 line) "" (Fasta name read : acc)
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
