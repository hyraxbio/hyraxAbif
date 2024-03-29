{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hyax.Abif.Write
Description : Functionality for writing AB1 files
Copyright   : (c) HyraxBio, 2018
License     : BSD3
Maintainer  : andre@hyraxbio.co.za, andre@andrevdm.com
Stability   : beta

Functionality for writing AB1 files.
See 'Hyrax.Abif.Generate.generateAb1' for an example of how to create an 'Ab1'
-}
module Hyrax.Abif.Write
    ( createAbifBytes
    , writeAbif
    , putAbif
    , putTextStr
    , putHeader
    , putDirectory
    , mkHeader
    , mkRoot
    , mkData
    , mkComment
    , mkSampleName
    , mkBaseOrder
    , mkLane
    , mkCalledBases
    , mkMobilityFileName
    , mkDyeSignalStrength
    , mkPeakLocations
    , addDirectory
    , Base (..)
    ) where

import           Verset
import qualified Data.Text as Txt
import qualified Data.Text.Encoding as TxtE
import qualified Data.Binary as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import           Hyrax.Abif

-- | Used to specify the base order for the FWO directry entry, see 'mkBaseOrder'
data Base = BaseA | BaseC | BaseG | BaseT


-- | Write an 'Abif' to a 'ByteString'
createAbifBytes :: Abif -> BSL.ByteString
createAbifBytes ab1 =
  B.runPut (putAbif ab1)

  
-- | Write an 'Abif' to a file
writeAbif :: FilePath -> Abif -> IO ()
writeAbif destPath ab1 = do
  let b = createAbifBytes ab1
  BS.writeFile destPath $ BSL.toStrict b

  
-- | Create the 'Abif' using "Data.Binary"
putAbif :: Abif -> B.Put
putAbif (Abif header root dirs) = do
  -- Total data size
  let dataSize = foldl' (\acc i -> if i > 4 then acc + i else acc) 0 $ dDataSize <$> dirs
  
  -- Write the header
  putHeader header

  -- Data starts at offset 128
  let startDataOffset = 128
  -- Write the root directory entry
  putDirectory (startDataOffset + dataSize) $ root { dDataSize = 28 * length dirs
                                                   , dElemNum = length dirs
                                                   }

  -- Write 47 zero Int16 values as required by the spec
  traverse_ B.putInt16be $ replicate 47 0
  -- Write the data, for all data larger than four bytes. Data four bytes or less is stored
  --  in the offset field
  traverse_ (B.putLazyByteString . dData) $ filter (\d -> dDataSize d > 4) dirs
  -- Write the directory entries. 
  foldM_ writeDir startDataOffset dirs

  where
    writeDir offset dir = do
      putDirectory offset dir
      pure $ if dDataSize dir > 4
             then offset + dDataSize dir
             else offset


-- | Write 'Text'
putTextStr :: Text -> B.Put
putTextStr t = B.putByteString $ TxtE.encodeUtf8 t


-- | Write a 'ElemPString'
putPStr :: Text -> B.Put
putPStr t = do
  B.putInt8 . fromIntegral $ Txt.length t
  B.putByteString $ TxtE.encodeUtf8 t


-- | Write a 'Header'
putHeader :: Header -> B.Put
putHeader h = do
  putTextStr $ hName h
  B.putInt16be . fromIntegral $ hVersion h


-- | Write a 'Directory'
putDirectory :: Int -> Directory -> B.Put
putDirectory dirOffset d = do
  let name = Txt.justifyLeft 4 ' ' . Txt.take 4 $ dTagName d
  putTextStr name
  B.putInt32be . fromIntegral $ dTagNum d
  B.putInt16be . fromIntegral $ dElemTypeCode d
  B.putInt16be . fromIntegral $ dElemSize d
  B.putInt32be . fromIntegral $ dElemNum d
  B.putInt32be . fromIntegral $ dDataSize d

  -- data with a size >= 4 are written in the offset
  if dDataSize d > 4
    then B.putInt32be . fromIntegral $ dirOffset
    else B.putLazyByteString . BSL.take 4 $ dData d <> "\0\0\0\0"

  B.putInt32be 0 -- reserved / datahandle


-- | Create a 'Header'
mkHeader :: Header
mkHeader =
  Header { hName = "ABIF"
         , hVersion = 101
         }


-- | Create the root 'Directory' entry
mkRoot :: Directory
mkRoot = 
  Directory { dTagName = "tdir"
                     , dTagNum = 1
                     , dElemTypeCode = 1023
                     , dElemTypeDesc = "root"
                     , dElemType = ElemRoot
                     , dElemSize = 28
                     , dDataOffset = 0
                     , dDataDebug = []
                     , dData = ""
                     , dDataSize = 0
                     , dElemNum = 0
                     }
  

-- | Create a comment 'Directory' entry and 'ElemPString' data
mkComment :: Text -> Directory
mkComment comment' = 
  let comment = B.runPut . putPStr $ comment' in

  Directory { dTagName = "CMNT" -- Comment
            , dTagNum = 1
            , dElemTypeCode = 18
            , dElemTypeDesc = "pString"
            , dElemType = ElemPString
            , dElemSize = 1
            , dElemNum = 1
            , dDataOffset = 0
            , dDataDebug = []
            , dData = comment
            , dDataSize = fromIntegral (BSL.length comment)
            } 


-- | Create a sample name (SMPL) 'Directory' entry and 'ElemPString' data
mkSampleName :: Text -> Directory
mkSampleName sampleName' =
  let sampleName = B.runPut . putPStr $ sampleName' in
  Directory { dTagName = "SMPL" -- Sample name
            , dTagNum = 1
            , dElemTypeCode = 18
            , dElemTypeDesc = "pString"
            , dElemType = ElemPString
            , dElemSize = 1
            , dElemNum = 10
            , dDataOffset = 0
            , dDataDebug = []
            , dData = sampleName
            , dDataSize = fromIntegral (BSL.length sampleName)
            }

-- | Create a base order (FWO_) 'Directory' entry data
mkBaseOrder :: Base -> Base -> Base -> Base -> Directory
mkBaseOrder w x y z =
  Directory { dTagName = "FWO_" -- Base order
            , dTagNum = 1
            , dElemTypeCode = 2
            , dElemTypeDesc = "char"
            , dElemType = ElemChar
            , dElemSize = 1
            , dDataOffset = 0
            , dDataDebug = []
            , dData = getBase w <> getBase x <> getBase y <> getBase z
            , dDataSize = 4
            , dElemNum = 4
            }
  where
    getBase BaseA = "A"
    getBase BaseC = "C"
    getBase BaseG = "G"
    getBase BaseT = "T"


-- | Create a lane (LANE) 'Directory' entry and data
mkLane :: Int16 -> Directory
mkLane lane =
  Directory { dTagName = "LANE" -- Lane or capliary number
            , dTagNum = 1
            , dElemTypeCode = 4
            , dElemTypeDesc = "short"
            , dElemType = ElemShort
            , dElemSize = 2
            , dElemNum = 1
            , dDataSize = 2
            , dDataOffset = 0
            , dData = B.runPut $ B.putInt16be lane
            , dDataDebug = []
            }


-- | Create a called bases (PBAS) 'Directory' entry and data
mkCalledBases :: Text -> Directory
mkCalledBases fasta = 
  let
    generatedFastaLen = Txt.length fasta
    pbas = BSL.fromStrict . TxtE.encodeUtf8 $ fasta
  in
  Directory { dTagName = "PBAS" -- Called bases
            , dTagNum = 1
            , dElemTypeCode = 2
            , dElemTypeDesc = "char"
            , dElemType = ElemChar
            , dElemSize = 1
            , dDataOffset = 0
            , dDataDebug = []
            , dData = pbas
            , dDataSize = generatedFastaLen
            , dElemNum = generatedFastaLen 
            }


-- | Create a mobility file name (PDMF) 'Directory' entry and 'ElemPString' data
mkMobilityFileName :: Int -> Text -> Directory
mkMobilityFileName tagNum fileName =
  let pdfm = B.runPut $ putPStr fileName in
  Directory { dTagName = "PDMF" -- Mobility file name
            , dTagNum = tagNum
            , dElemTypeCode = 18
            , dElemTypeDesc = "pString"
            , dElemType = ElemPString
            , dElemSize = 1
            , dDataOffset = 0
            , dDataDebug = []
            , dData = pdfm
            , dDataSize = fromIntegral (BSL.length pdfm)
            , dElemNum = fromIntegral (BSL.length pdfm) 
            }


-- | Create a signal strength (S/N%) 'Directory' entry and data
mkDyeSignalStrength :: Int16 -> Int16 -> Int16 -> Int16 -> Directory
mkDyeSignalStrength w x y z =
  let sigStrength = B.runPut $ do
        B.putInt16be w
        B.putInt16be x
        B.putInt16be y
        B.putInt16be z
  in
  Directory { dTagName = "S/N%" -- Signal strength per dye
            , dTagNum = 1
            , dElemTypeCode = 4
            , dElemTypeDesc = "short"
            , dElemType = ElemShort
            , dElemSize = 2
            , dElemNum = 4
            , dDataOffset = 0
            , dDataDebug = []
            , dData = sigStrength
            , dDataSize = fromIntegral (BSL.length sigStrength)
            }


-- | Create a peak locations (PLOC) 'Directory' entry and array of 'ElemShort' data
mkPeakLocations :: [Int16] -> Directory
mkPeakLocations locs =
  let peakLocations = B.runPut $ traverse_ B.putInt16be locs in
  Directory { dTagName = "PLOC" -- Peak locations
            , dTagNum = 1
            , dElemTypeCode = 4
            , dElemTypeDesc = "short"
            , dElemType = ElemShort
            , dElemSize = 2
            , dDataOffset = 0
            , dDataDebug = []
            , dData = peakLocations
            , dDataSize = fromIntegral $ BSL.length peakLocations
            , dElemNum = length locs
            }


-- | Create a data (DATA) 'Directory' entry and array of 'ElemShort' data
mkData :: Int -> [Int16] -> Directory
mkData tagNum ds =
  let ds' = B.runPut $ traverse_ B.putInt16be ds in
  Directory { dTagName = "DATA"
            , dTagNum = tagNum
            , dElemTypeCode = 4
            , dElemTypeDesc = "short"
            , dElemType = ElemShort
            , dElemSize = 2
            , dDataOffset = 0
            , dDataDebug = []
            , dData = ds'
            , dDataSize = fromIntegral (BSL.length ds')
            , dElemNum = length ds
            }

-- | Add a directory to an 'Abif'
addDirectory :: Abif -> Directory -> Abif
addDirectory abif dir =
  abif { aDirs = aDirs abif <> [dir] }
