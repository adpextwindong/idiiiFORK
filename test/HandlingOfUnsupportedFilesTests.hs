module HandlingOfUnsupportedFilesTests (tests) where

import Test.HUnit
import TestHelp (testFile)

import ID3 (readTag)

testTryingToReadMP3WithOldUnsupportedID3v2Tag = TestCase $ do
  fpath <- testFile "id3v2.2-tag.mp3"
  maybeTag <- readTag fpath
  assertEqual "We at least want to make sure the library doesn't crash" Nothing maybeTag

tests =
  [TestLabel "Make sure library doesn't crash when trying to read a pre-ID3v2.3 tag"
             testTryingToReadMP3WithOldUnsupportedID3v2Tag]
