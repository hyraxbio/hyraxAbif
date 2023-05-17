{-# LANGUAGE NoImplicitPrelude #-}

import           Verset
import qualified System.IO as IO

import qualified AbifTests
import qualified FastaTests

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering

  results <- sequence [ AbifTests.tests
                      , FastaTests.tests
                      ]

  if and results
    then pass
    else exitFailure
