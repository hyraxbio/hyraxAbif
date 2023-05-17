{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Description : Example showing how to add a comment to an existing AB1 file
Copyright   : (c) HyraxBio, 2018
License     : BSD3
Maintainer  : andre@hyraxbio.co.za, andre@andrevdm.com

Example showing how to add a comment to an existing AB1 file.
See other examples in "Examples"
-}
module Examples.AddComment where

import           Verset

import qualified Hyrax.Abif.Read as H
import qualified Hyrax.Abif.Write as H

-- | Add a comment to an existing AB1 file
addComment :: IO ()
addComment = do
  abif' <- H.readAbif "example.ab1"

  case abif' of
    Left e -> putStrLn $ "error reading ABIF: " <> e
    Right abif -> do
      let modified = H.addDirectory abif $ H.mkComment "new comment"
      H.writeAbif "example.modified.ab1" modified
