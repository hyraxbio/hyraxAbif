{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

{-|
Module      : Hyax.Abi
Description : Core AB1 types 
Copyright   : (c) HyraxBio, 2018
License     : BSD3
Maintainer  : andre@hyraxbio.co.za
Stability   : beta

This module contains the core types for working with AB1 files.

See

  * <https://github.com/hyraxbio/hyraxAbi/#readme Source code on github>

  * <http://www6.appliedbiosystems.com/support/software_community/ABIF_File_Format.pdf The ABIF spec>
-}
module Hyrax.Abi
    ( Abi (..)
    , Header (..)
    , Directory (..)
    , ElemType (..)
    , getElemType
    , describeElemType
    ) where

import           Protolude
import qualified Data.ByteString.Lazy as BSL


data Header = Header { hName :: !Text
                     , hVersion :: !Int
                     } deriving (Show, Eq)

data Directory = Directory { dTagName :: !Text
                           , dTagNum :: !Int
                           , dElemTypeCode :: !Int
                           , dElemTypeDesc :: !Text
                           , dElemType :: !ElemType
                           , dElemSize :: !Int
                           , dElemNum :: !Int
                           , dDataSize :: !Int
                           , dDataOffset :: !Int
                           , dData :: !BSL.ByteString
                           , dDataDebug :: ![Text]
                           } deriving (Show, Eq)

data Abi = Abi { aHeader :: !Header
               , aRootDir :: !Directory
               , aDirs :: ![Directory]
               } deriving (Show, Eq)


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


getElemType :: Int -> ElemType
getElemType e = fst $ describeElemType e

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
