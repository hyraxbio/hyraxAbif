{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Example showing how to add a comment to an existing AB1 file
module Examples.AddComment where

import           Protolude

import qualified Hyrax.Abi.Read as H
import qualified Hyrax.Abi.Write as H

-- | Add a comment to an existing AB1 file
addComment :: IO ()
addComment = do
  abi' <- H.readAbi "example.ab1"

  case abi' of
    Left e -> putStrLn $ "error reading ABI: " <> e
    Right abi -> do
      let modified = H.addDirectory abi $ H.mkComment "new comment"
      H.writeAbi "example.modified.ab1" modified
