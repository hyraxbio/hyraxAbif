{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module DemoSpec (tests) where


import           Protolude
import qualified Data.List as Lst
import qualified Data.Text as Txt
import qualified Data.Text.Encoding as TxtE
import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.ByteString.Lazy as BSL
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Hyrax.Abi as H
import qualified Hyrax.Abi.Read as H
import qualified Hyrax.Abi.Write as H

nucsGen :: (Monad m) => PropertyT m Text
nucsGen = 
  forAll $ Gen.text (Range.linear 1 1000) (Gen.element "ACGTMRWSYKVHDBNX")


nucsNoIupacGen :: (Monad m) => PropertyT m Text
nucsNoIupacGen = 
  forAll $ Gen.text (Range.linear 1 1000) (Gen.element "ACGT")


fastaGen :: (Monad m) => PropertyT m ByteString
fastaGen = do
  nucs <- nucsGen
  pure . TxtE.encodeUtf8 $ "> 1\n" <> nucs


prop_roundtrip :: Property
prop_roundtrip = property $ do 
  fasta <- fastaGen

  wfasta <- evalEither $ H.readWeightedFasta fasta
  let ab1Written1 = H.generateAb1 ("test", wfasta)
  ab1Read1 <- evalEither $ H.getAbi ab1Written1

  let ab1Written2 = H.writeAb1 ab1Read1
  ab1Read2 <- evalEither $ H.getAbi ab1Written2

  ab1Read1 === ab1Read2
  

prop_readPeaks :: Property
prop_readPeaks = property $ do
  nucs <- nucsNoIupacGen
  let fasta = TxtE.encodeUtf8 $ "> 1\n" <> nucs

  wfasta <- evalEither $ H.readWeightedFasta fasta
  let ab1Written = H.generateAb1 ("test", wfasta)
  ab1 <- evalEither $ H.getAbi ab1Written

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
    
    readDataPeaks :: H.Abi -> Int -> [Int] -> Either Text [Int]
    readDataPeaks ab1 dirNum peaks =
      case readShorts <$> getDirEntry ab1 "DATA" dirNum of
        Left e -> Left e
        Right vs ->
          let valsAtPeaks = atMay vs <$> peaks in
          maybeToRight "peaks" $ sequenceA valsAtPeaks
  

getDirEntry :: H.Abi -> Text -> Int -> Either Text BSL.ByteString
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
