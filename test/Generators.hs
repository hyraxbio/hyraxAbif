{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Generators where

import           Verset
import qualified Data.Text as Txt
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Hyrax.Abif.Fasta as F

-- | Generate random set of nucleotides, including ambiguous ones
nucsGen :: GenT Identity Text
nucsGen =
  Gen.text (Range.linear 1 1000) (Gen.element ("ACGTMRWSYKVHDBNX" :: [Char]))


-- | Generate random set of nucleotides, no ambiguous ones
nucsNoIupacGen :: (Monad m) => PropertyT m Text
nucsNoIupacGen =
  forAll $ Gen.text (Range.linear 1 1000) (Gen.element ("ACGT" :: [Char]))


-- | Generate data to construct a fasta "file"
--   List of tuples of (name, lines of reads)
--      maxReads controls the max number in the list
genFastaData :: Int -> Gen [(Int, [Text])]
genFastaData maxReads =
  Gen.list (Range.linear 1 maxReads) $
    (,) <$> Gen.int (Range.linear 1 9999) <*> Gen.list (Range.linear 1 15) nucsGen


-- | Convert fasta data to a single string
toFastaTxt :: Bool -> [(Int, [Text])] -> Text
toFastaTxt addSpaces is =
  Txt.intercalate "\n" $ toRead <$> is

  where
    toRead (weight, ls) =
      let sp = if addSpaces then "  " else "" in
      ">" <> sp <> show weight <> "\n" <> Txt.intercalate "\n" ls


-- | Convert fasta data to the expected list of FASTA values
toFasta :: [(Int, [Text])] -> [F.Fasta]
toFasta is =
  toRead <$> is

  where
    toRead (weight, ls) = F.Fasta (show weight) $ Txt.concat ls

