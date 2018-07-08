{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module FastaTests (tests) where

import           Protolude
import qualified Data.Text as Txt
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Hyrax.Abi.Fasta as F
import           AbiTests (nucsGen)

-- | Generate data to construct a fasta "file"
--   List of tuples of (name, lines of reads)
--      maxReads controls the max number in the list
genFastaData :: Int -> Gen [(Int, [Text])]
genFastaData maxReads = do
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


-- | Test parsing
prop_fasta :: Property
prop_fasta = property $ do
  withSpaces <- forAll $ Gen.element [True, False]
  fdata <- forAll $ genFastaData 3
  let ftxt = toFastaTxt withSpaces fdata
  let fasta = toFasta fdata
  let parsed = F.parseFasta ftxt
  parsed === Right fasta
  

tests :: IO Bool
tests =
  checkParallel $$discover
