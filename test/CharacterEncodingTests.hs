module CharacterEncodingTests (tests) where

import Test.HUnit
import TestHelp (assertTagHasGivenTextValues)

testReadingTagThatHasNonAsciiCharsWithISO88591Encoding = TestCase $ do
  assertTagHasGivenTextValues "iso88591-with-non-ascii-chars.mp3"
    [("TIT2", "ÿ÷ý->§É"), ("TPE1", "ÎOû")]

testReadingTagThatHasUTF16FrameInLittleEndianWithByteOrderMark = TestCase $ do
  assertTagHasGivenTextValues "utf16-in-little-endian-with-bom.mp3"
    [("TIT2", "ごめんなさい"), ("TPE1", "ö Mádè Ûp Ñame ö"), ("TALB", "µ ¾ Ψ ϖ ™ ⇑ ∀ ♣")]

testReadingTagThatHasUTF16FrameInBigEndianWithByteOrderMark = TestCase $
  assertTagHasGivenTextValues "utf16-in-big-endian-with-bom.mp3" [("TIT2", "Кофе")]

testReadingTagThatHasUTF16FrameWithoutByteOrderMark = TestCase $ assertTagHasGivenTextValues
  "utf16-without-bom.mp3" [("TIT2", "¤ ¥ $ € £")]

testUTF16FrameWhenNoNullTerminationCharacterIsUsed = TestCase $ assertTagHasGivenTextValues
  "utf16-with-no-null-termination-char.mp3"
  [("TIT2", "My Maria"), ("TYER", "1997"), ("TPUB", "Hey-ya")]

tests = [TestLabel ("test reading a tag that has a frame with text encoded as ISO-8859-1, with at " ++
              "least a portion of the characters in the non-ASCII range")
             testReadingTagThatHasNonAsciiCharsWithISO88591Encoding,
   TestLabel ("test reading a tag that has a frame with text encoded as UTF-16, containing a " ++
              "Byte-Order Mark (BOM), in little-endian byte order (0xFF 0xFE)")
             testReadingTagThatHasUTF16FrameInLittleEndianWithByteOrderMark,
   TestLabel ("test reading a tag that has a frame with text encoded as UTF-16, containing a " ++
              "Byte-Order Mark (BOM), in big-endian byte order (0xFE 0xFF)")
             testReadingTagThatHasUTF16FrameInBigEndianWithByteOrderMark,
   TestLabel ("test reading a tag that has a frame with text encoded as UTF-16, without a " ++
              "Byte-Order Mark (thus, in big-endian byte order as required by the standard)")
             testReadingTagThatHasUTF16FrameWithoutByteOrderMark,
   TestLabel ("test reading a tag that uses UTF-16 encoding but does not use a null " ++
              "character (\x00\x00) to terminate strings contained within frames")
             testUTF16FrameWhenNoNullTerminationCharacterIsUsed]
