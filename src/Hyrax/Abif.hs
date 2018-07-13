{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

{-|
Module      : Hyax.Abif
Description : Core AB1 types 
Copyright   : (c) HyraxBio, 2018
License     : BSD3
Maintainer  : andre@hyraxbio.co.za, andre@andrevdm.com
Stability   : beta

This module contains the core types for working with AB1 files.

See

  * <https://github.com/hyraxbio/hyraxAbif/#readme Source code on github>

  * <http://www6.appliedbiosystems.com/support/software_community/ABIF_File_Format.pdf The ABIF spec>
-}
{-! SECTION< abif_module !-}
module Hyrax.Abif
    ( Abif (..)
    , Header (..)
    , Directory (..)
    , ElemType (..)
    , getElemType
    , describeElemType
    ) where

import           Protolude
import qualified Data.ByteString.Lazy as BSL
{-! SECTION> abif_module !-}


{-! SECTION< abif_type !-}
-- | A single ABIF
data Abif = Abif { aHeader :: !Header
                 , aRootDir :: !Directory
                 , aDirs :: ![Directory]
                 } deriving (Show, Eq)


-- | ABIF header
data Header = Header { hName :: !Text
                     , hVersion :: !Int
                     } deriving (Show, Eq)

-- | ABIF directory entry.
-- The 'dData' field contains the data for the entry
data Directory = Directory { dTagName :: !Text        -- ^ Tag name
                           , dTagNum :: !Int          -- ^ Tag number, see e.g. how DATA entries use this
                           , dElemType :: !ElemType   -- ^ Type of an element
                           , dElemTypeCode :: !Int    -- ^ Integer value of 'dElemType'
                           , dElemTypeDesc :: !Text   -- ^ Description of 'dElemType'
                           , dElemSize :: !Int        -- ^ Size in bytes of each element
                           , dElemNum :: !Int         -- ^ Number of elements in the data. See the spec per data type. E.g. for a string this is the number of characters
                           , dDataSize :: !Int        -- ^ Number of bytes in the data
                           , dDataOffset :: !Int      -- ^ Offset of this directory entry's data in the file. For data that is four
                                                      --    bytes or less, the data itself is stored in this field.
                                                      --    This value will be recalculated when writing an ABIF so you do not need to manually set it.
                           , dData :: !BSL.ByteString -- ^ The entry's data
                           , dDataDebug :: ![Text]    -- ^ Optinal debug data, populated by 'Hyrax.Abif.Read.getDebug' when a ABIF is parsed
                           } deriving (Show, Eq)
{-! SECTION> abif_type !-}


{-! SECTION< abif_elemType !-}
-- | Type of the elements in a directory entry. See the spec for details on each type if required.
data ElemType = ElemUnknown
              | ElemCustom
              | ElemByte
              | ElemChar
              | ElemWord
              | ElemShort
              | ElemLong
              | ElemFloat
              | ElemDouble
              | ElemDate
              | ElemTime
              | ElemPString
              | ElemCString
              | ElemThumb
              | ElemBool
              | ElemRationalUnsupported
              | ElemBCDUnsupported
              | ElemPointUnsupported
              | ElemRectUnsupported
              | ElemVPointUnsupported
              | ElemVRectUnsupported
              | ElemTagUnsupported
              | ElemDeltaCompUnsupported
              | ElemLZWCompUnsupported
              | ElemCompressedDataUnsupported
              | ElemRoot
              deriving (Show, Eq)


-- | Get an 'ElemType' from a elem type code
getElemType :: Int -> ElemType
getElemType e = fst $ describeElemType e

-- | Get the description for an 'ElemType'
describeElemType :: Int -> (ElemType, Text)
describeElemType    1 = (ElemByte,    "byte")
describeElemType    2 = (ElemChar,    "char")
describeElemType    3 = (ElemWord,    "word")
describeElemType    4 = (ElemShort,   "short")
describeElemType    5 = (ElemLong,    "long")
describeElemType    7 = (ElemFloat,   "float")
describeElemType    8 = (ElemDouble,  "double")
describeElemType   10 = (ElemDate,    "date")
describeElemType   11 = (ElemTime,    "time")
describeElemType   18 = (ElemPString, "pString")
describeElemType   19 = (ElemCString, "cString")
describeElemType   12 = (ElemThumb,   "thumb")
describeElemType   13 = (ElemBool,    "bool")
describeElemType    6 = (ElemRationalUnsupported,       "rational (*unsupported*)")
describeElemType    9 = (ElemBCDUnsupported,            "BCD (*unsupported*)")
describeElemType   14 = (ElemPointUnsupported,          "point (*unsupported*)")
describeElemType   15 = (ElemRectUnsupported,           "rect (*unsupported*)")
describeElemType   16 = (ElemVPointUnsupported,         "vPoint (*unsupported*)")
describeElemType   17 = (ElemVRectUnsupported,          "vRect (*unsupported*)")
describeElemType   20 = (ElemTagUnsupported,            "Tag (*unsupported*)")
describeElemType  128 = (ElemDeltaCompUnsupported,      "deltaComp (*unsupported*)")
describeElemType  256 = (ElemLZWCompUnsupported,        "LZWComp (*unsupported*)")
describeElemType  384 = (ElemCompressedDataUnsupported, "Compressed Data (*unsupported*)")
describeElemType 1023 = (ElemRoot, "root")
describeElemType    v = if v >= 1024 then (ElemCustom, "custom") else (ElemUnknown, "unknown")
{-! SECTION> abif_elemType !-}
