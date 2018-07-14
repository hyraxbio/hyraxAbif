{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-|
Module      : Hyax.Abif.Generate
Description : Generate AB1 from a weighted FASTA
Copyright   : (c) HyraxBio, 2018
License     : BSD3
Maintainer  : andre@hyraxbio.co.za, andre@andrevdm.com

Functionality for generating AB1 files from an input FASTA. These AB1s are supported by both PHRED and recall,
if you are using other software you may need to add additional required sections.

= Weighted reads

The input FASTA files have "weighted" reads. The name for each read is an value between 0 and 1
 which specifies the height of the peak relative to a full peak. 


== Single read

The most simple example is a single FASTA with a single read with a weight of 1

@
> 1
ACTG
@

<<docs/eg_actg.png>>

The chromatogram for this AB1 shows perfect traces for the input `ACTG` nucleotides with a full height peak.


== Mixes & multiple reads 

The source FASTA can have multiple reads, which results in a chromatogram with mixes

@
> 1
ACAG
> 0.3
ACTG
@

<<docs/eg_acag_acgt_mix.png>>

There is an `AT` mix at the third nucleotide. The first read has a weight of 1 and the second a weight of 0.3.
The chromatogram shows the mix and the `T` with a lower peak (30% of the `A` peak)

== Summing weights

 - The weigh of a read specifies the intensity of the peak from 0 to 1. 
 - Weights for each position are added to a maximum of 1 per nucleotide
 - You can use `_` as a "blank" nucleotide, in which only the nucleotides from other reads will be considered

E.g.

@
> 1
ACAG
> 0.3
_GT
> 0.2
_G
@

<<docs/eg_multi_mix.png>>


== Reverse reads

A weighted FASTA can represent a reverse read. To do this add a `R` suffix to the weight.
The data you enter should be entered as if it was a forward read. This data will be complemented
and reversed before writing to the ABIF

E.g.

@
> 1R
ACAG
@

See README.md for additional details and examples
-}
module Hyrax.Abif.Generate
    ( generateAb1s
    , generateAb1
    , readWeightedFasta
    , iupac
    , unIupac
    , complementNucleotides
    ) where

import           Protolude
import qualified Data.Text as Txt
import qualified Data.Text.Encoding as TxtE
import qualified Data.List as Lst
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified System.FilePath as FP
import           System.FilePath ((</>))
import qualified System.Directory as Dir

import           Hyrax.Abif
import           Hyrax.Abif.Write
import           Hyrax.Abif.Fasta

data TraceData = TraceData { trData09G :: ![Int16]
                           , trData10A :: ![Int16]
                           , trData11T :: ![Int16]
                           , trData12C :: ![Int16]
                           , trValsPerBase :: !Int
                           , trFasta :: !Text
                           } deriving (Show)

-- | Generate a set of AB1s. One for every FASTA found in the source directory
generateAb1s :: FilePath -> FilePath -> IO ()
generateAb1s source dest = do
  Dir.createDirectoryIfMissing True dest
  weighted <- readWeightedFastas source

  case weighted of
    Left e -> putText e
    Right rs -> do
      let ab1s = (\(n, r) -> (n, generateAb1 (n, r))) <$> rs
      traverse_ (\(name, ab1) -> BS.writeFile (dest </> Txt.unpack name <> ".ab1") $ BSL.toStrict ab1) ab1s


{-! SECTION< gen_generateAb1_fn !-}
-- | Create the 'ByteString' data for an AB1 given the data from a weighted FASTA (see 'readWeightedFasta')
generateAb1 :: (Text, [(Double, Text)]) -> BSL.ByteString
generateAb1 (fName, sourceFasta) = 
  let
    tr = generateTraceData sourceFasta
    valsPerBase = trValsPerBase tr
    generatedFastaLen = (Txt.length $ trFasta tr)

{-! SECTION< gen_generateAb1_peak !-}
    -- The point that is the peak of the trace, i.e. mid point of trace for a single base
    midPeek = valsPerBase `div` 2
    -- Get the peak locations for all bases
    peakLocations = take generatedFastaLen [midPeek, valsPerBase + midPeek..]
{-! SECTION> gen_generateAb1_peak !-}

    -- Sample name (from the FASTA name)
    sampleName = fst . Txt.breakOn "_" $ fName

{-! SECTION< gen_generateAb1_abif !-}
    -- Create the ABIF directories
    dirs = [ mkData  9 $ trData09G tr -- G
           , mkData 10 $ trData10A tr -- A
           , mkData 11 $ trData11T tr -- T
           , mkData 12 $ trData12C tr -- C
           , mkBaseOrder BaseG BaseA BaseT BaseC -- Base order, should be GATC for 3500
           , mkLane 1 -- Lane or capliary number
           , mkCalledBases $ trFasta tr -- Called bases
           , mkMobilityFileName 1 "KB_3500_POP7_BDTv3.mob" -- Mobility file name
           , mkMobilityFileName 2 "KB_3500_POP7_BDTv3.mob" -- Mobility file name
           , mkPeakLocations $ fromIntegral <$> peakLocations -- Peak locations
           , mkDyeSignalStrength 53 75 79 48 -- Signal strength per dye
           , mkSampleName sampleName  -- Sample name
           , mkComment "Generated by HyraxBio AB1 generator"
           ]

    -- The ABIF
    abif = Abif { aHeader = mkHeader
                , aRootDir = mkRoot
                , aDirs = dirs
                }
{-! SECTION> gen_generateAb1_abif !-}
            
  in
  -- Generate the data
  B.runPut (putAbif abif)
{-! SECTION> gen_generateAb1_fn !-}


{-! SECTION< gen_generateTraceData !-}
{-! SECTION< gen_generateTraceData_type !-}
-- | Generate the traces for the AB1 from the parsed weighted FASTA
generateTraceData :: [(Double, Text)] -> TraceData
generateTraceData weighted =
{-! SECTION> gen_generateTraceData_type !-}
  let
{-! SECTION< gen_generateTraceData_weighted !-}
    weightedNucs' = (\(w, ns) -> (w,) . unIupac <$> Txt.unpack ns) <$> weighted
    weightedNucs = Lst.transpose weightedNucs'
{-! SECTION> gen_generateTraceData_weighted !-}
  
{-! SECTION< gen_generateTraceData_curve !-}
    -- Values for a base that was present. This defines the shape of the chromatogram curve,
    --  and defines the number of values per base
    curve = [0, 0, 128, 512, 1024, 1024, 512, 128, 0, 0]
    valsPerBase = length curve
{-! SECTION> gen_generateTraceData_curve !-}

{-! SECTION< gen_generateTraceData_traces !-}
    -- Create the G, A, T and C traces
    data09G = concat $ getWeightedTrace curve 'G' <$> weightedNucs
    data10A = concat $ getWeightedTrace curve 'A' <$> weightedNucs
    data11T = concat $ getWeightedTrace curve 'T' <$> weightedNucs
    data12C = concat $ getWeightedTrace curve 'C' <$> weightedNucs
{-! SECTION> gen_generateTraceData_traces !-}

{-! SECTION< gen_generateTraceData_fasta !-}
    -- Create fasta sequence for the trace
    fastaSeq = concat <$> (snd <<$>> weightedNucs)
    fasta = Txt.pack $ iupac fastaSeq
{-! SECTION> gen_generateTraceData_fasta !-}
  in      
{-! SECTION< gen_generateTraceData_ret !-}
  TraceData { trData09G = data09G
            , trData10A = data10A
            , trData11T = data11T
            , trData12C = data12C
            , trFasta = fasta
            , trValsPerBase = valsPerBase
            }
{-! SECTION> gen_generateTraceData_ret !-}

  where
{-! SECTION< gen_generateTraceData_getWeightedTrace !-}
    getWeightedTrace :: [Int] -> Char -> [(Double, [Char])] -> [Int16]
    getWeightedTrace curve nuc ws =
      let
        found = filter ((nuc `elem`) . snd) ws
        score' = foldl' (+) 0 $ fst <$> found
        score = min 1 . max 0 $ score'
        wave = floor . (score *) . fromIntegral <$> curve
      in
      wave
{-! SECTION> gen_generateTraceData_getWeightedTrace !-}
{-! SECTION> gen_generateTraceData !-}


-- | Read a weighted FASTA file. See the module documentation for details on the format of the weighted FASTA 
-- Reads with a weight followed by an `R` are reverse reads, and the AB1 generated will contain the complemeted
-- sequence.
--
-- e.g. weighted FASTA
--
-- @
-- > 1
-- ACAG
-- > 0.3
-- _GT
-- > 0.2
-- _G
-- @
--
--
-- The result data has the type
-- 
-- @
--   [('Double', 'Text')]
--     ^        ^
--     |        |
--     |        +---- read 
--     | 
--     +---- weight
-- @
--
{-! SECTION< gen_readWeightedFasta !-}
readWeightedFasta :: ByteString -> Either Text [(Double, Text)]
readWeightedFasta fastaData = 
  case parseFasta $ TxtE.decodeUtf8 fastaData of
    Left e -> Left e
    Right fs -> getWeightedFasta fs

  where
    getWeightedFasta :: [Fasta] -> Either Text [(Double, Text)]
    getWeightedFasta fs = 
      case sequenceA $ readWeighted <$> fs of
        Left e -> Left e
        Right r -> Right r

    readWeighted :: Fasta -> Either Text (Double, Text)
    readWeighted (Fasta hdr' dta) =
      let (processNucs, hdr) =
            -- If there is a 'R' suffix, then generate a reverse sequence
            --  Which means complement each nucleotide and then reverse the string
            if Txt.isSuffixOf "R" hdr'
            then (Txt.reverse . complementNucleotides, Txt.strip . Txt.dropEnd 1 $ hdr')
            else (identity, hdr')
      in
      
      case (readMaybe . Txt.unpack $ hdr :: Maybe Double) of
        Just weight -> Right (min 1 . max 0 $ weight, processNucs $ Txt.strip dta)
        Nothing -> Left $ "Invalid header reading, expecting numeric weight, got: " <> hdr
{-! SECTION> gen_readWeightedFasta !-}

  

{-! SECTION< gen_readWeightedFastas !-}
-- | Read all FASTA files in a directory
--
-- The result data has the type
-- 
-- @
--                    [ ('Text', [('Double', 'Text')]) ]
--                        ^         ^         ^
--                        |         |         |
-- file name -------------+         |         +---- read 
--                                  | 
--                                  +---- weight
-- @
--
readWeightedFastas :: FilePath -> IO (Either Text [(Text, [(Double, Text)])])
readWeightedFastas source = do
  files <- filter (Txt.isSuffixOf ".fasta" . Txt.pack) <$> getFiles source
  let names = Txt.pack . FP.takeBaseName <$> files
  contents <- traverse BS.readFile files
  
  case sequenceA $ readWeightedFasta <$> contents of
    Left e -> pure . Left $ e
    Right rs -> pure . Right $ zip names rs
{-! SECTION> gen_readWeightedFastas !-}

  
-- | Find all files in a directory
getFiles :: FilePath -> IO [FilePath]
getFiles p = do
  entries <- (p </>) <<$>> Dir.listDirectory p
  filterM Dir.doesFileExist entries


{-! SECTION< gen_unIupac_fn !-}
-- | Convert a IUPAC ambiguity code to the set of nucleotides it represents
{-! SECTION< gen_unIupac_type !-}
unIupac :: Char -> [Char]
{-! SECTION> gen_unIupac_type !-}
unIupac c =
  case c of
    'T' -> "T"
    'C' -> "C"
    'A' -> "A"
    'G' -> "G"
   
    'U' -> "T"
    'M' -> "AC"
    'R' -> "AG"
    'W' -> "AT"
    'S' -> "CG"
    'Y' -> "CT"
    'K' -> "GT"
    'V' -> "ACG"
    'H' -> "ACT"
    'D' -> "AGT"
    'B' -> "CGT"
    'N' -> "GATC"
  
    'X' -> "GATC"
    _   -> ""
{-! SECTION> gen_unIupac_fn !-}


-- | Given a set of nucleotides get the IUPAC ambiguity code
iupac :: [[Char]] -> [Char]
iupac ns =
  go <$> ns

  where
    go cs =
      let
        a = 'A' `elem` cs
        c = 'C' `elem` cs
        g = 'G' `elem` cs
        t = 'T' `elem` cs
      in
      case (a, c, g, t) of
        (True,  False, False, False) -> 'A'
        (False, True,  False, False) -> 'C'
        (False, False, True,  False) -> 'G'
        (False, False, False, True ) -> 'T'
        (True,  True,  False, False) -> 'M'
        (True,  False, True,  False) -> 'R'
        (True,  False, False, True ) -> 'W'
        (False, True,  True,  False) -> 'S'
        (False, True,  False, True ) -> 'Y'
        (False, False, True,  True ) -> 'K'
        (True,  True,  True,  False) -> 'V'
        (True,  True,  False, True ) -> 'H'
        (True,  False, True,  True ) -> 'D'
        (False, True,  True,  True ) -> 'B'
        (True,  True,  True,  True ) -> 'N'
        _ -> '_'


-- | Return the complement of a nucelotide string
complementNucleotides :: Text -> Text
complementNucleotides ns =
  let
    un = unIupac <$> Txt.unpack ns
    comp = complementNuc <<$>> un
    iu = iupac comp
  in
  Txt.pack iu

  where
    complementNuc 'A' = 'T'
    complementNuc 'G' = 'C'
    complementNuc 'T' = 'A'
    complementNuc 'C' = 'G'
    complementNuc x = x
