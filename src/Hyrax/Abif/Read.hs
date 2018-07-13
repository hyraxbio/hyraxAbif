{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

{-|
Module      : Hyax.Abif.Read
Description : Read and parse AB1 files
Copyright   : (c) HyraxBio, 2018
License     : BSD3
Maintainer  : andre@hyraxbio.co.za, andre@andrevdm.com
Stability   : beta

Functionality for reading and parsing AB1 files

e.g.

@
abif' <- readAbif "example.ab1"

case abif' of
  Left e -> putStrLn $ "error reading ABIF: " <> e
  Right abif -> print $ clearAbif abif
@
-}
module Hyrax.Abif.Read
    ( readAbif
    , getAbif
    , clear
    , clearAbif
    , getDebug
    , getPString
    , getCString
    , getHeader
    , getRoot
    , getDirectories
    , getDirectory
    ) where

import           Protolude
import qualified Data.Text as Txt
import qualified Data.Text.Encoding as TxtE
import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.ByteString.Lazy as BSL
import           Control.Monad.Fail (fail)

import           Hyrax.Abif


{-! SECTION< read_readAbif !-}
-- | Read and parse an AB1 file
readAbif :: FilePath -> IO (Either Text Abif)
readAbif path = getAbif <$> BSL.readFile path
{-! SECTION> read_readAbif !-}


{-! SECTION< read_getAbif !-}
-- | Parse an AB1 from a 'ByteString'
getAbif :: BSL.ByteString -> Either Text Abif
getAbif bs = do
  (header, rootDir) <- case B.runGetOrFail (getRoot bs) bs of
                         Right (_, _, x) -> pure x
                         Left (_, _, e) -> Left ("Error reading root: " <> Txt.pack e)

  let dirBytes = BSL.drop (fromIntegral $ dDataOffset rootDir) bs
  
  ds <- case B.runGetOrFail (getDirectories bs [] $ dElemNum rootDir) dirBytes of
          Right (_, _, x) -> pure x
          Left (_, _, e) -> Left ("Error reading "
                                  <> show (dElemNum rootDir)
                                  <> " directories (at " <> show (dDataOffset rootDir) <> "): "
                                  <> Txt.pack e
                                 )
  
  pure $ Abif header rootDir ds
{-! SECTION> read_getAbif !-}


-- | Removes all data from the ABIF's directories
clearAbif :: Abif -> Abif
clearAbif a = a { aRootDir = clear $ aRootDir a
               , aDirs = clear <$> aDirs a
               }


-- | Removes all data from a directory entry. This will probably only be useful when trying to show an ABIF value
clear :: Directory -> Directory
clear d = d { dData = "" }


-- | Populate the directory entry with debug data (into 'dDataDebug').
-- This is done for selected types only, e.g. for strings so that printing the structure will display
-- readable/meaningfull info
getDebug :: Directory -> Directory
getDebug d =
  let bsAtOffset = dData d in
  
  case dElemType d of
    -- Strings have a count = number of chars, not number of "strings"
    ElemPString ->
      if dDataSize d <= 4
      then d { dDataDebug = [TxtE.decodeUtf8 . BSL.toStrict . BSL.drop 1 . BSL.take (fromIntegral $ dDataSize d) $ dData d] }
      else d { dDataDebug = [B.runGet (lbl getPString) bsAtOffset] }

    -- Strings have a count = number of chars, not number of "strings"
    ElemCString ->
      if dDataSize d <= 4
      then d { dDataDebug = [TxtE.decodeUtf8 . BSL.toStrict . BSL.take (fromIntegral $ dDataSize d - 1) $ dData d] }
      else d { dDataDebug = [B.runGet (lbl . getCString $ dDataSize d) bsAtOffset] }

    y ->
      -- For non-array entries
      if dElemNum d == 1
      then 
        case y of
          ElemDate -> 
            flip B.runGet (dData d) $ lbl $ do
              yy <- B.getInt16be
              mt <- B.getInt8
              dt <- B.getInt8
              pure d { dDataDebug = [show yy <> "/" <> show mt <> "/" <> show dt]}
             
          ElemTime ->
            flip B.runGet (dData d) $ lbl $ do
              hr <- B.getInt8
              mn <- B.getInt8
              sc <- B.getInt8
              ss <- B.getInt8
              pure $ d { dDataDebug = [show hr <> ":" <> show mn <> ":" <> show sc <> "." <> show ss] }
             
          ElemLong ->
            flip B.runGet (dData d) $ lbl $ do
              x <- B.getInt32be
              pure $ d { dDataDebug =  [show x] }
             
          ElemShort ->
            flip B.runGet (dData d) $ lbl $ do
              x <- B.getInt16be
              pure $ d { dDataDebug = [show x] }
             
          ElemFloat ->
            flip B.runGet (dData d) $ lbl $ do
              x <- B.getFloatbe
              pure $ d { dDataDebug = [show x] }
             
          ElemWord ->
            flip B.runGet (dData d) $ lbl $ do
              x <- B.getInt8
              pure $ d { dDataDebug = [show x] }
             
          ElemChar ->
            flip B.runGet (dData d) $ lbl $ do
              x <- B.getWord8
              let c = BSL.pack [x]
              pure $ d { dDataDebug = [TxtE.decodeUtf8 . BSL.toStrict $ c] }
              
          _ -> d
      else
        case y of
          ElemChar -> -- Array of chars can be treated as a string
            flip B.runGet (dData d) $ lbl $ do
              cs <- readArray B.getWord8
              case dTagName d of
                "PCON" -> pure d { dDataDebug = [show cs] }
                _ -> do
                  let c = BSL.pack cs
                  pure $ d { dDataDebug = [TxtE.decodeUtf8 . BSL.toStrict $ c] }

          --ElemShort ->
          --  flip B.runGet (dData d) $ lbl $ do
          --    xs <- readArray B.getInt16be
          --    pure $ d { dDataDebug = [show xs] }

          _ -> d -- Do nothing

  where
    lbl = B.label $ "Reading " <> show (dElemTypeDesc d) <> " data size=" <> show (dDataSize d) <> " dir entry=" <> Txt.unpack (dTagName d) <> " cached data size=" <> (show . BSL.length $ dData d) <> ". "

    readArray :: B.Get n -> B.Get [n]
    readArray getFn = do
      e <- B.isEmpty
      if e then return []
      else do
        c <- getFn
        cs <- readArray getFn
        pure (c:cs)


-- | Parse a 'ElemPString'
getPString :: B.Get Text
getPString = do
  sz <- fromIntegral <$> B.getInt8
  TxtE.decodeUtf8 <$> B.label ("PString length=" <> show sz <> ".") (B.getByteString sz)


-- | Parse a 'ElemCString'
getCString :: Int -> B.Get Text
getCString sz = 
  TxtE.decodeUtf8 <$> B.getByteString (sz - 1)


-- | Parse the ABIF 'Header'
getHeader :: B.Get Header
getHeader = 
  Header <$> (TxtE.decodeUtf8 <$> B.getByteString 4)
         <*> (fromIntegral <$> B.getInt16be)


-- | Parse the root ('Header' and 'Directory')
getRoot :: BSL.ByteString -> B.Get (Header, Directory)
getRoot bs = do
  h <- getHeader
  rd <- getDirectory bs
  pure (h, rd)


-- | Parse a single 'Directory' entry and read its data
getDirectory :: BSL.ByteString -> B.Get Directory
getDirectory bs = do
  tagName <- TxtE.decodeUtf8 <$> B.getByteString 4
  tagNum <- fromIntegral <$> B.getInt32be
  typeCode <- fromIntegral <$> B.getInt16be
  elemSize <- fromIntegral <$> B.getInt16be
  elemNum <- fromIntegral <$> B.getInt32be
  dataSize <- fromIntegral <$> B.getInt32be
  offsetDataBytes <- B.lookAhead $ B.getLazyByteString 4
  dataOffset <- fromIntegral <$> B.getInt32be

  -- Read the data
  --  Data that is 4 bytes or less is stored in the offset field
  dataBytes <- if dataSize <= 4
                    then pure $ BSL.take (fromIntegral dataSize) offsetDataBytes
                    else case B.runGetOrFail (B.getLazyByteString $ fromIntegral dataSize) $ BSL.drop (fromIntegral dataOffset) bs of
                           Right (_, _, x) -> pure x
                           Left (_, _, e) -> fail $ "error reading data (" <> show dataSize <> " bytes starting at " <> show dataOffset <> ") for directory entry '" <> Txt.unpack tagName <> "': " <> e

  let (elemType, elemCode) = describeElemType typeCode
  pure Directory { dTagName = tagName 
                 , dTagNum = tagNum 
                 , dElemTypeCode = typeCode 
                 , dElemTypeDesc = elemCode 
                 , dElemType = elemType 
                 , dElemSize = elemSize 
                 , dElemNum = elemNum 
                 , dDataSize = dataSize 
                 , dDataOffset = dataOffset 
                 , dData = dataBytes 
                 , dDataDebug = []
                 } 


-- | Parse all the directoy entries
getDirectories :: BSL.ByteString -> [Directory] -> Int -> B.Get [Directory]
getDirectories _ acc 0 = pure acc
getDirectories bs acc more = do
  d <- getDirectory bs
  B.skip 4 -- Skip the reserved field
  getDirectories bs (acc <> [d]) (more - 1)


