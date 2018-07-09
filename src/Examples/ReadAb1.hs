{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Description : Example of reading a AB1 file
Copyright   : (c) HyraxBio, 2018
License     : BSD3
Maintainer  : andre@hyraxbio.co.za, andre@andrevdm.com

Example of reading a AB1 file.
See other examples in "Examples"
-}
module Examples.ReadAb1 where

import           Protolude

import qualified Hyrax.Abi.Read as H

-- | Read and print a AB1 file
addComment :: IO ()
addComment = do
  abi' <- H.readAbi "example.ab1"

  case abi' of
    Left e -> putStrLn $ "error reading ABI: " <> e
    Right abi ->
      -- Print after removing the data, to make it readable
      print $ H.clearAbi abi
