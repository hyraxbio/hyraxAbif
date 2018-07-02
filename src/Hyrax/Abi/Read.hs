{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Hyrax.Abi.Read
    ( readAbi
    , getAbi
    , clean
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

import           Hyrax.Abi


readAbi :: FilePath -> IO (Either Text Abi)
readAbi path = 
  getAbi <$> BSL.readFile path


getAbi :: BSL.ByteString -> Either Text Abi
getAbi bs = do
  (header, rootDir) <- case B.runGetOrFail (getRoot bs) bs of
                         Right (_, _, x) -> pure x
                         Left (_, _, e) -> Left ("Error reading root: " <> Txt.pack e)

  let dirBytes = BSL.drop (fromIntegral $ dDataOffset rootDir) bs
  
  ds <- case B.runGetOrFail (getDirectories bs [] $ dElemNum rootDir) dirBytes of
          Right (_, _, x) -> pure x
          Left (_, _, e) -> Left ("Error reading " <> show (dElemNum rootDir) <> " directories (at " <> show (dDataOffset rootDir) <> "): " <> Txt.pack e)
  
  pure $ Abi header rootDir ds


clean :: Directory -> Directory
clean d = d { dData = "" }


getDebug :: Directory -> Directory
getDebug d =
  let bsAtOffset = dData d in
  
  case dElemType d of
    ElemPString ->
      if dDataSize d <= 4
      then d { dDataDebug = [TxtE.decodeUtf8 . BSL.toStrict . BSL.drop 1 . BSL.take (fromIntegral $ dDataSize d) $ dData d] }
      else d { dDataDebug = [B.runGet (lbl getPString) bsAtOffset] }

    ElemCString ->
      if dDataSize d <= 4
      then d { dDataDebug = [TxtE.decodeUtf8 . BSL.toStrict . BSL.take (fromIntegral $ dDataSize d - 1) $ dData d] }
      else d { dDataDebug = [B.runGet (lbl . getCString $ dDataSize d) bsAtOffset] }

    y ->
      if dElemNum d == 1
      then 
        case y of
          ElemDate -> --TODO create datetime
            flip B.runGet (dData d) $ lbl $ do
              yy <- B.getInt16be
              mt <- B.getInt8
              dt <- B.getInt8
              pure d { dDataDebug = [show yy <> "/" <> show mt <> "/" <> show dt]}
             
          ElemTime -> --TODO create datetime
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
          ElemChar ->
            flip B.runGet (dData d) $ lbl $ do
              cs <- readArray B.getWord8
              let c = BSL.pack cs
              pure $ d { dDataDebug = [TxtE.decodeUtf8 . BSL.toStrict $ c] }

          --ElemShort ->
          --  flip B.runGet (dData d) $ lbl $ do
          --    xs <- readArray B.getInt16be
          --    pure $ d { dDataDebug = [show xs] }

          _ -> d

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


getPString :: B.Get Text
getPString = do
  sz <- fromIntegral <$> B.getInt8
  TxtE.decodeUtf8 <$> B.label ("PString length=" <> show sz <> ".") (B.getByteString sz)


getCString :: Int -> B.Get Text
getCString sz = 
  TxtE.decodeUtf8 <$> B.getByteString (sz - 1)


getHeader :: B.Get Header
getHeader = 
  Header <$> (TxtE.decodeUtf8 <$> B.getByteString 4)
         <*> (fromIntegral <$> B.getInt16be)


getRoot :: BSL.ByteString -> B.Get (Header, Directory)
getRoot bs = do
  h <- getHeader
  rd <- getDirectory bs
  pure (h, rd)


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


getDirectories :: BSL.ByteString -> [Directory] -> Int -> B.Get [Directory]
getDirectories _ acc 0 = pure acc
getDirectories bs acc more = do
  d <- getDirectory bs
  B.skip 4
  getDirectories bs (acc <> [d]) (more - 1)


