{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Protolude
import qualified Data.Text as Txt
import qualified Data.ByteString.Lazy as BSL
import           Text.Show.Pretty (ppShow)
import qualified System.Environment as Env
import qualified Language.Haskell.HsColour as Clr
import qualified Language.Haskell.HsColour.Colourise as Clr

import qualified Hyrax.Abif as H
import qualified Hyrax.Abif.Read as H
import qualified Hyrax.Abif.Generate as H

main :: IO ()
main = do
  args <- Env.getArgs 

  case headMay args of
    Just "dump" -> runDump
    Just "gen" -> runGenerateAb1
    _ -> putText "unknown args, expecting dump/gen"
  

runGenerateAb1 :: IO ()
runGenerateAb1 =
  Env.getArgs >>= \case
    [_, source, dest] -> H.generateAb1s source dest
    _ -> putText "Expecting `source dest` args"
  

runDump :: IO ()
runDump =
  Env.getArgs >>= \case
    [_, path] -> do
      file <- BSL.readFile path
      case H.getAbif file of
        Left e -> putText e
        Right (H.Abif hdr root dirs) -> do
          let debugged = H.clear . H.getDebug <$> dirs 
          --colourPrint debugged 
          colourPrint hdr
          colourPrint . H.clear $ root
          colourPrint debugged

          putText . Txt.intercalate "\n" $
            (\d -> H.dTagName d
              <> " {" <> H.dElemTypeDesc d <> "} tagNum="
              <> show (H.dTagNum d)
              <> " size=" <> show (H.dElemSize d)
              <> " count=" <> show (H.dElemNum d)
              <> " offset=" <> show (H.dDataOffset d)
              <> "  "
              <> show (H.dDataDebug d)
            ) <$> debugged
    _ ->
      putText "Expecting path to ab1"


myColourPrefs :: Clr.ColourPrefs
myColourPrefs = Clr.defaultColourPrefs { Clr.conop    = [Clr.Foreground Clr.Yellow]
                                       , Clr.conid    = [Clr.Foreground Clr.Yellow, Clr.Bold]
                                       , Clr.string   = [Clr.Foreground $ Clr.Rgb 29 193 57]
                                       , Clr.char     = [Clr.Foreground Clr.Cyan]
                                       , Clr.number   = [Clr.Foreground $ Clr.Rgb 202 170 236]
                                       , Clr.keyglyph = [Clr.Foreground Clr.Yellow]
                                       }
                
colourPrint :: (Show a) => a -> IO ()
colourPrint = putStrLn . Clr.hscolour Clr.TTY myColourPrefs False False [] False . ppShow
