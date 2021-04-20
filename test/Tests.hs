module Main where

import Test.HUnit

import qualified CharacterEncodingTests
import qualified FrameSupportTests
import qualified HandlingOfUnsupportedFilesTests
import qualified HeaderTests
import qualified UnsynchronisationHandlingTests

main :: IO ()
main = do
  let tests = 
        CharacterEncodingTests.tests ++
        FrameSupportTests.tests ++
        HandlingOfUnsupportedFilesTests.tests ++
        HeaderTests.tests ++
        UnsynchronisationHandlingTests.tests
  runTestTT $ TestList tests
  return ()
