module UnsynchronisationHandlingTests (tests) where

import Test.HUnit
import TestHelp (assertTagHasGivenTextValues)

testReadingID3v2v3TagThatHasUnsynchronisationFlagOffWithLargeFrame = TestCase $
  assertTagHasGivenTextValues "large-comm-frame-with-unsync-not-used.mp3"
    [("TCON", "Pranks"), ("COMM", reallyLongComment), ("TIT2", "Clip from \"Marcus Cinemas\""),
     ("TPE1", "Funny Pranks")]
  where reallyLongComment = ("This is just going to be a really long comment, not really " ++
                             "useful for anything other than testing, but it needs to be at " ++
                             "least quite a bit larger than the maximum value of a single " ++
                             "byte, because we need to test that ID3v2.3 tags are read " ++
                             "properly when they don't have their \"unsynchronisation\" flag " ++
                             "turned on (i.e., when it's turned off), because if the ID3v2 " ++
                             "tag-decoder still does the \"unsynchronisation decoding\" it " ++
                             "will, clearly, not properly read the ID3v2 tag!")

testReadingID3v2v4TagThatWithLargeFrame = TestCase $
  assertTagHasGivenTextValues "large-comm-frame-with-id3v2.4.mp3"
    [("TIT2", "Clip from \"Marcus Cinemas\""), ("TPE1", "Funny Pranks"),
     ("TALB", reallyLongComment), ("TDRC", "2001"), ("TCON", "Pranks"),
     ("COMM", reallyLongComment)]
  where reallyLongComment =
          "And this is also just a big-old, long-dong comment, not really useful for anything " ++
          "other than testing, but it STILL needs to be at least a bit larger than the " ++
          "maximum value that a single byte can hold, because we need to test that ID3v2.4 " ++
          "tags are read properly with regard to \"unsynchronisation\"."

tests = 
  [TestLabel "Make sure we properly handle ID3v2.3 tags that have \"unsynchronisation\" flag off"
             testReadingID3v2v3TagThatHasUnsynchronisationFlagOffWithLargeFrame,
   TestLabel "Make sure we properly handle \"unsynchronisation\" for ID3v2.4 tags"
             testReadingID3v2v4TagThatWithLargeFrame]
