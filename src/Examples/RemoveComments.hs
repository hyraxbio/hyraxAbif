{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Description : Example showing how to remove all comments from an ABIF file
Copyright   : (c) HyraxBio, 2018
License     : BSD3
Maintainer  : andre@hyraxbio.co.za, andre@andrevdm.com

Example showing how to remove all comments from an ABIF file.
See other examples in "Examples"
-}
module Examples.RemoveComments where

import           Protolude

import qualified Hyrax.Abif as H
import qualified Hyrax.Abif.Read as H
import qualified Hyrax.Abif.Write as H

-- | Remove all comments from an existing file
removeComments :: IO ()
removeComments = do
  abif' <- H.readAbif "example.ab1"

  case abif' of
    Left e -> putStrLn $ "error reading ABIF: " <> e
    Right abif -> do
      let modified = abif { H.aDirs = filter noComments $ H.aDirs abif }
      H.writeAbif "example.modified.ab1" modified

  where
    noComments :: H.Directory -> Bool
    noComments dir = H.dTagName dir /= "CMNT"
