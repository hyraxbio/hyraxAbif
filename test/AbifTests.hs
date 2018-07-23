{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module AbifTests (tests) where


import           Protolude
import qualified Data.List as Lst
import qualified Data.Text as Txt
import qualified Data.Text.Encoding as TxtE
import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.ByteString.Lazy as BSL
import           Hedgehog

import qualified Hyrax.Abif as H
import qualified Hyrax.Abif.Read as H
import qualified Hyrax.Abif.Write as H
import qualified Hyrax.Abif.Generate as H
import           Generators


-- | Test that complementing nucleotides is reversible
prop_complementNucs :: Property
prop_complementNucs = property $ do
  nucs' <- forAll $ nucsGen
  let nucs = Txt.replace "X" "N" nucs'

  let n1 = H.complementNucleotides nucs
  let n2 = Txt.replace "X" "N" $ H.complementNucleotides n1

  nucs === n2

  
-- | Test that an ab1 (write, read, write, read) results in the original data
prop_roundtrip :: Property
prop_roundtrip = property $ do 
  fdata <- forAll $ genFastaData 2
  let fasta = TxtE.encodeUtf8 $ toFastaTxt False fdata

  wfasta <- evalEither $ H.readWeightedFasta fasta
  let ab1Written1 = H.generateAb1 ("test", wfasta)
  ab1Read1 <- evalEither $ H.getAbif ab1Written1

  let ab1Written2 = H.createAbifBytes ab1Read1
  ab1Read2 <- evalEither $ H.getAbif ab1Written2

  ab1Read1 === ab1Read2
  
  
-- | Generate an ab1 from a fasta and then confirm that the generated peaks of the
--    chromatogram match the original fasta.
--    Note that we are only testing the simple/single fasta case (i.e. no mixes)
prop_readPeaks :: Property
prop_readPeaks = property $ do
  nucs <- nucsNoIupacGen
  let fasta = TxtE.encodeUtf8 $ "> 1\n" <> nucs

  wfasta <- evalEither $ H.readWeightedFasta fasta
  let ab1Written = H.generateAb1 ("test", wfasta)
  ab1 <- evalEither $ H.getAbif ab1Written

  -- Get the peak locations
  peaks <- evalEither $ readShorts <$> getDirEntry ab1 "PLOC"  1

  -- Get the val at the peak per channel
  chanG <- evalEither $ readDataPeaks ab1  9 peaks
  chanA <- evalEither $ readDataPeaks ab1 10 peaks
  chanT <- evalEither $ readDataPeaks ab1 11 peaks
  chanC <- evalEither $ readDataPeaks ab1 12 peaks

  -- Check that all the data is available
  length chanA === length peaks
  length chanC === length peaks
  length chanG === length peaks
  length chanT === length peaks

  -- Call the peaks
  let called = Txt.pack $ Lst.zipWith4 callPeaks chanA chanC chanG chanT

  -- Compare original fasta vs called
  called === nucs

  -- Compare to the sequence in the AB1
  pbas <- evalEither $ getDirEntry ab1 "PBAS" 1
  (TxtE.decodeUtf8 . BSL.toStrict $ pbas) === nucs

  where
    -- Highest peak wins, no iupac
    callPeaks :: Int -> Int -> Int -> Int -> Char
    callPeaks a c g t =
      case reverse $ Lst.sortOn snd [('A', a), ('C', c), ('G', g), ('T', t)] of
        ((n,_) : _) -> n
        _ -> '?'
    
    readDataPeaks :: H.Abif -> Int -> [Int] -> Either Text [Int]
    readDataPeaks ab1 dirNum peaks =
      case readShorts <$> getDirEntry ab1 "DATA" dirNum of
        Left e -> Left e
        Right vs ->
          let valsAtPeaks = atMay vs <$> peaks in
          maybeToRight "peaks" $ sequenceA valsAtPeaks
  

getDirEntry :: H.Abif -> Text -> Int -> Either Text BSL.ByteString
getDirEntry ab1 dirName dirNum =
  let r = filter (\d -> H.dTagNum d == dirNum && H.dTagName d == dirName) $ H.aDirs ab1 in
  case r of
    (a:_) -> Right $ H.dData a
    _ -> Left $ "No entry found for '" <> dirName <> "' " <> show dirNum


readShorts :: BSL.ByteString -> [Int]
readShorts =
  let r = B.runGet $ readArray B.getWord16be in
  fromIntegral <<$>> r


readArray :: B.Get n -> B.Get [n]
readArray getFn = do
  e <- B.isEmpty
  if e then return []
  else do
    c <- getFn
    cs <- readArray getFn
    pure (c:cs)



tests :: IO Bool
tests =
  checkParallel $$discover
