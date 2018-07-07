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

prop_fastaWithSpaces :: Property
prop_fastaWithSpaces = property $ do
  nucs <- forAll nucsGen
  weight <- forAll $ Gen.int (Range.linear 1 9999)
  let fasta = ">    " <> show weight <> " \n" <> nucs
  let parsed = F.parseFasta fasta
  parsed === Right [ F.Fasta { F._fastaName = show weight
                             , F._fastaRead = nucs
                             }
                   ]
  

prop_fastaWithNoSpaces :: Property
prop_fastaWithNoSpaces = property $ do
  nucs <- forAll nucsGen
  weight <- forAll $ Gen.int (Range.linear 1 9999)
  let fasta = ">" <> show weight <> "\n" <> nucs
  let parsed = F.parseFasta fasta
  parsed === Right [ F.Fasta { F._fastaName = show weight
                             , F._fastaRead = nucs
                             }
                   ]
  

prop_fastaMultiLine :: Property
prop_fastaMultiLine = property $ do
  nucsLines1 <- forAll $ Gen.list (Range.linear 1 15) nucsGen
  let nucs1 = Txt.intercalate "\n" nucsLines1
  let nucs1' = Txt.replace "\n" "" nucs1
  weight1 <- forAll $ Gen.int (Range.linear 1 9999)
  let fasta1 = ">" <> show weight1 <> "\n" <> nucs1

  nucsLines2 <- forAll $ Gen.list (Range.linear 1 15) nucsGen
  let nucs2 = Txt.intercalate "\n" nucsLines2
  let nucs2' = Txt.replace "\n" "" nucs2
  weight2 <- forAll $ Gen.int (Range.linear 1 9999)
  let fasta2 = ">" <> show weight2 <> "\n" <> nucs2

  let parsed = F.parseFasta $ fasta1 <> "\n" <> fasta2

  parsed === Right [ F.Fasta { F._fastaName = show weight2
                             , F._fastaRead = nucs2'
                             }
                   , F.Fasta { F._fastaName = show weight1
                             , F._fastaRead = nucs1'
                             }
                   ]

tests :: IO Bool
tests =
  checkParallel $$discover
