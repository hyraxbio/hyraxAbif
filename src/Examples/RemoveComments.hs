{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Description : Example showing how to remove all comments from a file
Copyright   : (c) HyraxBio, 2018
License     : BSD3
Maintainer  : andre@hyraxbio.co.za, andre@andrevdm.com

Example showing how to remove all comments from a file.
See other examples in "Examples"
-}
module Examples.RemoveComments where

import           Protolude

import qualified Hyrax.Abi as H
import qualified Hyrax.Abi.Read as H
import qualified Hyrax.Abi.Write as H

-- | Remove all comments from an existing file
removeComments :: IO ()
removeComments = do
  abi' <- H.readAbi "example.ab1"

  case abi' of
    Left e -> putStrLn $ "error reading ABI: " <> e
    Right abi -> do
      let modified = abi { H.aDirs = filter noComments $ H.aDirs abi }
      H.writeAbi "example.modified.ab1" modified

  where
    noComments :: H.Directory -> Bool
    noComments dir = H.dTagName dir /= "CMNT"
