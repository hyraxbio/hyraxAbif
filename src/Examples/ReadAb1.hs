{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Description : Example of reading a ABIF file
Copyright   : (c) HyraxBio, 2018
License     : BSD3
Maintainer  : andre@hyraxbio.co.za, andre@andrevdm.com

Example of reading a ABIF file.
See other examples in "Examples"
-}
module Examples.ReadAb1 where

import           Verset

import qualified Hyrax.Abif.Read as H

-- | Read and print a ABIF file
readAbif :: IO ()
readAbif = do
  abif' <- H.readAbif "example.ab1"

  case abif' of
    Left e -> putStrLn $ "error reading ABIF: " <> e
    Right abif ->
      -- Print after removing the data, to make it readable
      print $ H.clearAbif abif
