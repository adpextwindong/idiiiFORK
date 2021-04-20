module HeaderTests (tests) where

import Test.HUnit
import TestHelp (assertTagHasGivenTextValues)

-- This test does not actually assert that we properly make use of or read the extended
-- header, but only that a tag containing an extended header still leads to proper reading
-- of the frames contained within said tag.
testReadingTagThatHasExtendedHeader = TestCase $ do
  assertTagHasGivenTextValues "has-extended-header.mp3"
    [("TCON", "Pranks"), ("TPE1", "Funny Pranks")]

tests =
  [TestLabel "test that a tag containing an extended header can be read"
             testReadingTagThatHasExtendedHeader]
