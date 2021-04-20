module FrameSupportTests (tests) where

import Test.HUnit
import TestHelp (assertTagHasGivenTextValues, assertTagHasGivenFrames)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import ID3.Type.FrameInfo (FrameInfo(UFID, Text))

testReadingTagWithTYERAndTDATFrames = TestCase $ assertTagHasGivenTextValues
  "has-tyer-and-tdat-frames.mp3"
  [("TCON", "Pranks"), ("TIT2", "Clip from \"Marcus Cinemas\""), ("TPE1", "Funny Pranks"),
   ("COMM", "http://somefunnypranks.com")]

testReadingTagWithTSIZFrame = TestCase $ assertTagHasGivenTextValues
  "has-tsiz-frame.mp3"
  [("TCON", "Pranks"), ("TIT2", "Clip from \"Marcus Cinemas\""), ("TPE1", "Funny Pranks"),
   ("COMM", "http://somefunnypranks.com")]

testReadingTagWithUFIDFrame = TestCase $ assertTagHasGivenFrames
  "has-ufid-frame.mp3" [("UFID", UFID "http://musicbrainz.org" mbid), ("TDRC", Text 0x03 "1990")]
  where mbid = BS.unpack $ Char8.pack "d1d8610b-7727-4f98-82fb-e87e1a238a80"

testReadingTagWithPRIVFrame = TestCase $ assertTagHasGivenTextValues
  "has-priv-frame.mp3" [("TIT2", "Short clip from \"Mark Twain\"")]

testReadingTagWithMCDIFrame = TestCase $ assertTagHasGivenTextValues
  "has-mcdi-frame.mp3" [("TIT2", "Clip from \"Marcus Cinemas\""), ("TPE1", "Funny Pranks")]

testReadingWXXXFrameThatHasDescriptionEncodedInUTF16 = TestCase $ assertTagHasGivenTextValues
  "wxxx-with-desc-in-utf16.mp3"
  [("WXXX", "http://somefunnypranks.com"), ("TIT2", "Clip from \"Marcus Cinemas\"")]

testReadingTagWithUnsupportedFramesPresent = TestCase $ assertTagHasGivenTextValues
  "has-unsupported-frames.mp3" [("TPE1", "Funny Pranks")]

testReadingTagWithIllegalNewlineCharactersInFrame = TestCase $ assertTagHasGivenTextValues
  "has-illegal-newline.mp3" [("TCON", "Very Funny\r\nSuper Funny"), ("TPE1", "Funny Pranks")]

tests =
  [TestLabel "Support for 'TYER' and 'TDAT' frames" testReadingTagWithTYERAndTDATFrames,
   TestLabel "Support for 'TSIZ' frame" testReadingTagWithTSIZFrame,
   TestLabel "Support for 'UFID' frame" testReadingTagWithUFIDFrame,
   TestLabel "Support for 'PRIV' frame" testReadingTagWithPRIVFrame,
   TestLabel "Support for 'MCDI' frame" testReadingTagWithMCDIFrame,
   TestLabel "Support for 'WXXX' frame that uses UTF-16 to encode its description"
             testReadingWXXXFrameThatHasDescriptionEncodedInUTF16,
   TestLabel "Support for reading tags with unsupported frames"
             testReadingTagWithUnsupportedFramesPresent,
   TestLabel "Support for reading tags with illegal newline character(s) in frame"
             testReadingTagWithIllegalNewlineCharactersInFrame]
