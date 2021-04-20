-- | This Module provides parsers for frames.
module ID3.Parser.Frame
where

---- IMPORTS
import Text.ParserCombinators.Poly.State

import ID3.Parser.General
import ID3.Parser.NativeFrames
import ID3.Type.Frame
import ID3.Type.Header (unsynchFlag)

import Data.Accessor
import Data.Bits (testBit)
import Data.Either (rights)
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C
----

parseFrames :: TagParser ([FrameID], Map FrameID ID3Frame)
parseFrames = do
    framesAndErrs <- many1 anyFrame
    let frames = rights framesAndErrs
    let idList = map (\f -> f^.frHeader^.frID) frames
    return ( idList , Map.fromList (zip idList frames) )

-- | Parses any Frame Header
anyFrameHeader :: TagParser (Either String FrameHeader)
anyFrameHeader = do
    i <- frameID       `err` "frame id"
    s <- frameSize     `err` "frame size"
    if s > 500000
      then return $ Left $ "Frame of type " ++ (show i) ++ " is too big!"
      else do
        f <- frameFlags    `err` "frame flags"
        return $ Right $ initFrameHeader [frID^=i, frSize^=s, frFlags^=f]

-- | Parses any Frame
anyFrame :: TagParser (Either String ID3Frame)
anyFrame = do
    hdr <- anyFrameHeader `err` "frame header"
    case hdr of
      Left e -> return $ Left e
      Right h -> do
        i <- frameInfo (h^.frID) `err` "frame info"
        return $ Right $ initID3Frame [frHeader^=h, frInfo^=i]

frameID :: TagParser FrameID
frameID = do
    name <- count (4::Integer) $ upper `onFail` digit
    posUpdate (+4)
    return $ C.unpack $ BS.pack name

frameSize :: TagParser FrameSize
frameSize = do
    version <- tagVersionGet
    let (majorVersion, _) = version
    flags <- flagsGet
    let doDecoding = (majorVersion >= 4 || unsynchFlag flags)
    size <- parseSize 4 doDecoding
    return size

frameFlags :: TagParser FrameFlags
frameFlags = do
    s <- frameStatusFlags  `err` "status flags"
    f <- frameFormatFlags  `err` "format flags"
    return $ initFrameFlags [statusFlags^=s, formatFlags^=f]

frameStatusFlags :: Parser St Token StatusFlags
frameStatusFlags = do
    (v, _) <- tagVersionGet
    if v < 3 
      then return $ StatusFlags v (False, False, False)
      else do
        flags <- anyWord8
        sizeInc
        let bit = testBit flags
        case v of
          3 ->
            return $ StatusFlags v (bit 7, bit 6, bit 5) -- i.e., %abc00000
          4 ->
            return $ StatusFlags v (bit 6, bit 5, bit 4) -- i.e., %0abc0000
          _ ->
            error "internal error: status flag bits for unknown version"

frameFormatFlags :: Parser St Token FormatFlags
frameFormatFlags = do
    (v, _) <- tagVersionGet
    if v < 3 
      then return $ FormatFlags v (False, False, False, False, False)
      else do
        flags <- anyWord8
        sizeInc
        let bit = testBit flags
        case v of
          3 ->
            return $ FormatFlags v (bit 5, bit 7, bit 6, False, False) -- i.e., %ijk00000
          4 ->
            return $ FormatFlags v (bit 6, bit 3, bit 2, bit 1, bit 0) -- i.e., %0h00kmnp
          _ ->
            error "internal error: format flag bits for unknown version"


-- {-- FRAME CONTENT
{-
 -If nothing else is said, strings, including numeric strings and URLs
 -   [URL], are represented as ISO-8859-1 [ISO-8859-1] characters in the
 -   range $20 - $FF. Such strings are represented in frame descriptions
 -   as <text string>, or <full text string> if newlines are allowed. If
 -   nothing else is said newline character is forbidden. In ISO-8859-1 a
 -   newline is represented, when allowed, with $0A only.
 -
 -   Frames that allow different types of text encoding contains a text
 -   encoding description byte. Possible encodings:
 -
 -     $00   ISO-8859-1 [ISO-8859-1]. Terminated with $00.
 -     $01   UTF-16 [UTF-16] encoded Unicode [UNICODE] with BOM. All
 -           strings in the same frame SHALL have the same byteorder.
 -           Terminated with $00 00.
 -     $02   UTF-16BE [UTF-16] encoded Unicode [UNICODE] without BOM.
 -           Terminated with $00 00.
 -     $03   UTF-8 [UTF-8] encoded Unicode [UNICODE]. Terminated with $00.
 -
 -   Strings dependent on encoding are represented in frame descriptions
 -   as <text string according to encoding>, or <full text string
 -   according to encoding> if newlines are allowed. Any empty strings of
 -   type $01 which are NULL-terminated may have the Unicode BOM followed
 -   by a Unicode NULL ($FF FE 00 00 or $FE FF 00 00).
 -
 -   The timestamp fields are based on a subset of ISO 8601. When being as
 -   precise as possible the format of a time string is
 -   yyyy-MM-ddTHH:mm:ss (year, "-", month, "-", day, "T", hour (out of
 -   24), ":", minutes, ":", seconds), but the precision may be reduced by
 -   removing as many time indicators as wanted. Hence valid timestamps
 -   are
 -   yyyy, yyyy-MM, yyyy-MM-dd, yyyy-MM-ddTHH, yyyy-MM-ddTHH:mm and
 -   yyyy-MM-ddTHH:mm:ss. All time stamps are UTC. For durations, use
 -   the slash character as described in 8601, and for multiple non-
 -   contiguous dates, use multiple strings, if allowed by the frame
 -   definition.
 -
 -   The three byte language field, present in several frames, is used to
 -   describe the language of the frame's content, according to ISO-639-2
 -   [ISO-639-2]. The language should be represented in lower case. If the
 -   language is not known the string "XXX" should be used.
 -
 -   All URLs [URL] MAY be relative, e.g. "picture.png", "../doc.txt".
 -
 -   If a frame is longer than it should be, e.g. having more fields than
 -   specified in this document, that indicates that additions to the
 -   frame have been made in a later version of the ID3v2 standard. This
 -   is reflected by the revision number in the header of the tag.
 --}
