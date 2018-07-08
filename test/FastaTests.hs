{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module FastaTests (tests) where

import           Protolude
import           Hedgehog
import qualified Hedgehog.Gen as Gen

import qualified Hyrax.Abi.Fasta as F
import           Generators

-- | Test fasta parsing
prop_fasta :: Property
prop_fasta = property $ do
  withSpaces <- forAll $ Gen.element [True, False]
  fdata <- forAll $ genFastaData 8
  let ftxt = toFastaTxt withSpaces fdata
  let fasta = toFasta fdata
  let parsed = F.parseFasta ftxt
  parsed === Right fasta
  

tests :: IO Bool
tests =
  checkParallel $$discover
