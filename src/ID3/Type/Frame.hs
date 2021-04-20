module ID3.Type.Frame where

import Data.Accessor
import Data.Accessor.Basic (compose)
import ID3.Type.Flags
import ID3.Type.Unparse
import ID3.Type.FrameInfo
import Numeric
import Data.Char
import Data.Word

{-- | /ID3V2 FRAME OVERVIEW/

    All ID3v2 frames consists of one frame header followed by one or more
   fields containing the actual information. The header is always 10
   bytes and laid out as follows:

@
     Frame ID      $xx xx xx xx  (four characters)
     Size      4 * %0xxxxxxx
     Flags         $xx xx
@
--}


data ID3Frame = ID3Frame
                { frHeader_  :: FrameHeader    -- ^ frame Header
                , frInfo_    :: FrameInfo      -- ^ frame Information Value
                }   deriving Eq

emptyID3Frame :: ID3Frame
emptyID3Frame = ID3Frame emptyFrameHeader (Unknown [])
initID3Frame :: [ID3Frame -> ID3Frame] -> ID3Frame
initID3Frame  = flip compose emptyID3Frame

frHeader :: Accessor ID3Frame FrameHeader
frHeader = accessor frHeader_ (\x fr -> fr {frHeader_ = x})
frInfo :: Accessor ID3Frame FrameInfo
frInfo   = accessor frInfo_   (\x fr -> fr {frInfo_   = x})

instance HasSize ID3Frame where
    size = accessor (toInteger . length . unparse . getVal frInfo) (setVal $ frHeader .> frSize)

--content :: (FrameInfo -> a) -> Accessor ID3Frame a
--content a = accessor (\f -> a (f^.frInfo)) (\x f -> frInfo^=((f^.frInfo) {a=x}) $ f)
textContent :: Accessor ID3Frame String
textContent  = frInfo .> infoTextContent

instance Show ID3Frame where
    show fr = (show $ fr^.frHeader) ++"\n"++
              (show $ fr^.frInfo  )++"\n"

{-showFrameInfo info = concatMap showRec info
        where showRec (s, bs) = "\t"++s++":\t"++bs++"\n"-}

instance Parsed ID3Frame where
    unparse fr = (unparse $ fr^.frHeader) ++
                 (unparse $ fr^.frInfo  )

type FrameName = String

{-type FrameInfo = [(String, String)]-}

{- | Frame Header
-}
data FrameHeader = FrameHeader
                { frID_    :: FrameID         -- ^ frame ID
                , frSize_  :: FrameSize       -- ^ frame Size
                , frFlags_ :: FrameFlags      -- ^ frame Flags
                }   deriving Eq

emptyFrameHeader :: FrameHeader
emptyFrameHeader = FrameHeader "" 0 emptyFrameFlags
initFrameHeader :: [FrameHeader -> FrameHeader] -> FrameHeader
initFrameHeader  = flip compose emptyFrameHeader

frID :: Accessor FrameHeader FrameID
frID    = accessor frID_    (\x h -> h {frID_    = x})
frSize :: Accessor FrameHeader FrameSize
frSize  = accessor frSize_  (\x h -> h {frSize_  = x})
frFlags :: Accessor FrameHeader FrameFlags
frFlags = accessor frFlags_ (\x h -> h {frFlags_ = x})


instance Show FrameHeader where
    show (FrameHeader i s fs) = "\tFrame ID:\t"++i++"\n"++
                                "\tFrame size:\t"++(show s)++
                                (show fs)

instance Parsed FrameHeader where
    unparse fh = (unparse $ Str $ fh^.frID      ) ++
                 (unparse $ fh^.frSize          ) ++
                 (unparse $ fh^.frFlags         )

{-- | /FRAME ID/

    The frame ID is made out of the characters capital A-Z and 0-9.
   Identifiers beginning with "X", "Y" and "Z" are for experimental
   frames and free for everyone to use, without the need to set the
   experimental bit in the tag header. Bear in mind that someone else
   might have used the same identifier as you. All other identifiers are
   either used or reserved for future use.
--}
type FrameID = String

{-- | /SIZE BYTES/

   The frame ID is followed by a size descriptor containing the size of
   the data in the final frame, after encryption, compression and
   unsynchronisation. The size is excluding the frame header ('total
   frame size' - 10 bytes) and stored as a 32 bit synchsafe integer.
--}
type FrameSize = Integer

{-- | /Frame header flags/

   In the frame header the size descriptor is followed by two flag
   bytes. All unused flags MUST be cleared. The first byte is for
   'status messages' and the second byte is a format description. If an
   unknown flag is set in the first byte the frame MUST NOT be changed
   without that bit cleared. If an unknown flag is set in the second
   byte the frame is likely to not be readable. Some flags in the second
   byte indicates that extra information is added to the header. These
   fields of extra information is ordered as the flags that indicates
   them. The flags field is defined as follows (l and o left out because
   ther resemblence to one and zero):

@
     %0abc0000 %0h00kmnp
@

   Some frame format flags indicate that additional information fields
   are added to the frame. This information is added after the frame
   header and before the frame data in the same order as the flags that
   indicates them. I.e. the four bytes of decompressed size will precede
   the encryption method byte. These additions affects the 'frame size'
   field, but are not subject to encryption or compression.

   The default status flags setting for a frame is, unless stated
   otherwise, 'preserved if tag is altered' and 'preserved if file is
   altered', i.e. @%00000000@.
--}
data FrameFlags = FrameFlags
                  { statusFlags_ :: StatusFlags   -- ^ Frame status flags
                  , formatFlags_ :: FormatFlags   -- ^ Frame format flags
                  } deriving Eq

emptyFrameFlags :: FrameFlags
emptyFrameFlags = FrameFlags (StatusFlags 4 (False, False, False))
                             (FormatFlags 4 (False, False, False, False, False))
initFrameFlags :: [FrameFlags -> FrameFlags] -> FrameFlags
initFrameFlags  = flip compose emptyFrameFlags

statusFlags :: Accessor FrameFlags StatusFlags
statusFlags = accessor statusFlags_ (\x fs -> fs {statusFlags_ = x})
formatFlags :: Accessor FrameFlags FormatFlags
formatFlags = accessor formatFlags_ (\x fs -> fs {formatFlags_ = x})

instance Show FrameFlags where
  show fs = if not ((anyStatusFlagsOn $ fs^.statusFlags) || (anyFormatFlagsOn $ fs^.formatFlags))
            then "" else
              "\tFlags " ++ (showBinary $ unparse $ fs^.statusFlags) ++
              " " ++ (showBinary $ unparse $ fs^.formatFlags) ++ ":\n"++
              (showStatusFlags $ fs^.statusFlags) ++ "\n" ++
              (showFormatFlags $ fs^.formatFlags)

instance Parsed FrameFlags where
    unparse fs = (unparse $ fs^.statusFlags) ++ (unparse $ fs^.formatFlags)

{-- | /Frame status flags/

    Format: @%0abc0000@ where

   a - /Tag alter preservation/

     This flag tells the tag parser what to do with this frame if it is
     unknown and the tag is altered in any way. This applies to all
     kinds of alterations, including adding more padding and reordering
     the frames.

@
     0     Frame should be preserved.
     1     Frame should be discarded.
@

   b - /File alter preservation/

     This flag tells the tag parser what to do with this frame if it is
     unknown and the file, excluding the tag, is altered. This does not
     apply when the audio is completely replaced with other audio data.

@
     0     Frame should be preserved.
     1     Frame should be discarded.
@

   c - /Read only/

      This flag, if set, tells the software that the contents of this
      frame are intended to be read only. Changing the contents might
      break something, e.g. a signature. If the contents are changed,
      without knowledge of why the frame was flagged read only and
      without taking the proper means to compensate, e.g. recalculating
      the signature, the bit MUST be cleared.
--}
data StatusFlags = StatusFlags Word8 (Bool, Bool, Bool) deriving Eq

frameDiscardFlag :: StatusFlags -> Bool
frameDiscardFlag (StatusFlags _ (a, _, _)) = a
fileDiscardFlag :: StatusFlags -> Bool
fileDiscardFlag  (StatusFlags _ (_, b, _)) = b
readOnlyFlag :: StatusFlags -> Bool
readOnlyFlag     (StatusFlags _ (_, _, c)) = c
anyStatusFlagsOn :: StatusFlags -> Bool
anyStatusFlagsOn (StatusFlags _ (a, b, c)) = a || b || c

showBinary :: [Word8] -> String
showBinary [n] = 
  pad $ showIntAtBase 2 intToDigit n ""
  where
    pad s = replicate (8 - length s) '0' ++ s
showBinary _ = error "internal error: flags unparsed incorrectly"

showStatusFlags :: StatusFlags -> String
showStatusFlags stat = if not (anyStatusFlagsOn stat) then "" else "\t\tStatus Flags:\n" ++ finfo
  where finfo = (if frameDiscardFlag stat then "\t\t\t- Frame should be discarded when tag is altered\n" else "") ++
                (if fileDiscardFlag stat  then "\t\t\t- Frame should be discarded when file contents are altered\n" else "") ++
                (if readOnlyFlag stat     then "\t\t\t- Frame is read only!\n"                else "")

instance Parsed StatusFlags where
    unparse (StatusFlags v (a, b, c)) = 
      case v of
        3 -> [flagsToWord8 (a, b, c, False, False, False, False, False)]
        4 -> [flagsToWord8 (False, a, b, c, False, False, False, False)]
        _ -> [0]

{-- | /Frame format flags/

    Format: @%0h00kmnp@ where

   h - /Grouping identity/

      This flag indicates whether or not this frame belongs in a group
      with other frames. If set, a group identifier byte is added to the
      frame. Every frame with the same group identifier belongs to the
      same group.

@
      0     Frame does not contain group information
      1     Frame contains group information
@

   k - /Compression/

      This flag indicates whether or not the frame is compressed.
      A 'Data Length Indicator' byte MUST be included in the frame.

@
      0     Frame is not compressed.
      1     Frame is compressed using zlib [zlib] deflate method.
            If set, this requires the 'Data Length Indicator' bit
            to be set as well.
@

   m - /Encryption/

      This flag indicates whether or not the frame is encrypted. If set,
      one byte indicating with which method it was encrypted will be
      added to the frame. See description of the ENCR frame for more
      information about encryption method registration. Encryption
      should be done after compression. Whether or not setting this flag
      requires the presence of a 'Data Length Indicator' depends on the
      specific algorithm used.

@
      0     Frame is not encrypted.
      1     Frame is encrypted.
@

   n - /Unsynchronisation/

      This flag indicates whether or not unsynchronisation was applied
      to this frame. See section 6 for details on unsynchronisation.
      If this flag is set all data from the end of this header to the
      end of this frame has been unsynchronised. Although desirable, the
      presence of a 'Data Length Indicator' is not made mandatory by
      unsynchronisation.

@
      0     Frame has not been unsynchronised.
      1     Frame has been unsyrchronised.
@

   p - /Data length indicator/

      This flag indicates that a data length indicator has been added to
      the frame. The data length indicator is the value one would write
      as the 'Frame length' if all of the frame format flags were
      zeroed, represented as a 32 bit synchsafe integer.
@
      0      There is no Data Length Indicator.
      1      A data length Indicator has been added to the frame.
@
--}
data FormatFlags = FormatFlags Word8 (Bool, Bool, Bool, Bool, Bool) deriving Eq

groupPartFlag :: FormatFlags -> Bool
groupPartFlag     (FormatFlags _ (h, _, _, _, _)) = h    -- Grouping identity
compressedFlag :: FormatFlags -> Bool
compressedFlag    (FormatFlags _ (_, k, _, _, _)) = k    -- Compression
encryptedFlag :: FormatFlags -> Bool
encryptedFlag     (FormatFlags _ (_, _, m, _, _)) = m    -- Encryption
unsychronisedFlag :: FormatFlags -> Bool
unsychronisedFlag (FormatFlags _ (_, _, _, n, _)) = n    -- Unsynchronisation
dataLengthIdFlag :: FormatFlags -> Bool
dataLengthIdFlag  (FormatFlags _ (_, _, _, _, p)) = p    -- Data length indicator
anyFormatFlagsOn :: FormatFlags -> Bool
anyFormatFlagsOn  (FormatFlags _ (h, k, m, n, p)) = h || k || m || n || p

showFormatFlags :: FormatFlags -> String
showFormatFlags fs = if not (anyFormatFlagsOn fs) then "" else finfo
  where finfo = "\t\tFormat Flags:\n" ++ (concat $ map (\i -> "\t\t\t- " ++ i ++ "\n") items)
        items = [] ++
                (if groupPartFlag     fs then ["frame is a part of group"]        else []) ++
                (if compressedFlag    fs then ["frame is compressed"]             else []) ++
                (if encryptedFlag     fs then ["frame is encrypted"]              else []) ++
                (if unsychronisedFlag fs then ["frame is unsynchronised"]         else []) ++
                (if dataLengthIdFlag  fs then ["frame has data length indicator"] else [])

instance Parsed FormatFlags where
    unparse (FormatFlags v (h, k, m, n, p)) =
      case v of
        3 -> [flagsToWord8 (k, m, h, False, False, False, False, False)]
        4 -> [flagsToWord8 (False, h, False, False, k, m, n, p)]
        _ -> [0]

--------------------------------------------------------------------------------------------


initFrame :: FrameID -> ID3Frame
initFrame frid = updateSize $ initID3Frame [ frHeader.>frID ^= frid, frInfo ^= inf ]
    where inf = case frid of
            "UFID" -> UFID "" []
            "TXXX" -> TXXX 03 "" ""
            "TCMP" -> TCMP False
            ('T':_)-> Text 03 ""
            "WXXX" -> WXXX 03 "" ""
            ('W':_)-> URL  ""
            "MCDI" -> MCDI []
            --ETCO -- { format :: Integer
                -- , events :: [Event]} -- TODO
            --MLLT -- TODO
            --SYTC -- TODO
            "USLT" -> USLT 03 "eng" "" ""
            --SYLT enc lang timeFormat content descr
            "COMM" -> COMM 03 "eng" "" ""
            --RVA2 -- TODO
            --EQU2 -- TODO
            --RVRB -- TODO
            "APIC" -> APIC 03 "" 00 "" []
            --GEOB -- TODO
            "PCNT" -> PCNT 00
            "POPM" -> POPM "" 00 00
            --RBUF -- TODO
            --AENC -- TODO
            --LINK -- TODO
            --POSS -- TODO
            "USER" -> USER 03 "eng" ""
            --OWNE -- TODO
            --COMR -- TODO
            --ENCR -- TODO
            --GRID -- TODO
            "PRIV" -> PRIV "" []
            --SIGN -- TODO
            --ASPI -- TODO
            _      -> Unknown []
