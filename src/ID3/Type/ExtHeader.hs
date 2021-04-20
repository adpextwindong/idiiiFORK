module ID3.Type.ExtHeader (ID3ExtHeader, initID3ExtHeader, extSize, isUpdate, crcData,
                           restrictionsPresent) where

import Data.Accessor
import Data.Accessor.Basic (compose)
import ID3.Type.Flags (flagsToWord8)
import ID3.Type.Unparse

import Data.Word    (Word8)

{- | /EXTENDED HEADER OVERVIEW/ (optional)

    The extended header contains information that can provide further
   insight in the structure of the tag, but is not vital to the correct
   parsing of the tag information; hence the extended header is
   optional.

@
     Extended header size   4 * %0xxxxxxx
     Number of flag bytes       $01
     Extended Flags             $xx
@

   Where the 'Extended header size' is the size of the whole extended
   header, stored as a 32 bit synchsafe integer. An extended header can
   thus never have a size of fewer than six bytes.
-}
data ID3ExtHeader = ID3ExtHeader
                    { extSize_  :: Integer         -- ^ size of extended header
                    , isUpdate_ :: Bool            -- ^ is this tag an update?
                    , crcData_  :: Maybe [Word8]   -- ^ CRC data, if present

                    -- XXX: No support for actually reading restriction bits yet??
                    , restrictionsPresent_ :: Bool -- ^ Are tag restrictions present?
                    --, extFlags_ :: ExtFlags  -- ^ flags
                    } deriving Eq

-- | Forming empty value to return, if there is no Extended Header
emptyID3ExtHeader :: ID3ExtHeader
emptyID3ExtHeader = ID3ExtHeader 0 False Nothing False
initID3ExtHeader :: [ID3ExtHeader -> ID3ExtHeader] -> ID3ExtHeader
initID3ExtHeader = flip compose emptyID3ExtHeader

extSize :: Accessor ID3ExtHeader Integer
extSize  = accessor extSize_  (\x e -> e {extSize_ = x})
isUpdate :: Accessor ID3ExtHeader Bool
isUpdate = accessor isUpdate_ (\x e -> e {isUpdate_ = x})
crcData :: Accessor ID3ExtHeader (Maybe [Word8])
crcData  = accessor crcData_  (\x e -> e {crcData_ = x})
restrictionsPresent :: Accessor ID3ExtHeader Bool
restrictionsPresent = accessor restrictionsPresent_ (\x e -> e {restrictionsPresent_ = x})

--extFlags = accessor extFlags_ (\x e -> e {extFlags_ = x})

instance Show ID3ExtHeader where
    show eh = if (eh^.extSize) == 0 then "" else
                ("Extended Header:\n" ++
                 "Size: " ++ (show $ eh^.extSize) ++ " bytes\n" ++
                 "Extended Flags:\n" ++
                 (if eh^.isUpdate            then "\t- This tag is an update\n"          else "") ++
                 (if eh^.crcData /= Nothing  then "\t- There is some CRC data...\n"      else "") ++
                 (if eh^.restrictionsPresent then "\t- There are some restrictions...\n" else ""))
                 --(show $ eh^.extFlags)

instance Parsed ID3ExtHeader where
    unparse eh = (unparse $ eh^.extSize) ++
                 [0x01, (flagsToWord8 (False, eh^.isUpdate, eh^.crcData /= Nothing,
                                       eh^.restrictionsPresent, False, False, False, False))] ++
                 --(unparse $ initFlags [ (accessFlag 2)^=(eh^.extFlags^.isUpdate         )
                 --                     , (accessFlag 3)^=(eh^.extFlags^.crc          == [])
                 --                     , (accessFlag 4)^=(eh^.extFlags^.restrictions == [])
                 --                     ]) ++
                 (if eh^.isUpdate then [0x00] else []) ++
                 (case eh^.crcData of { Nothing -> []; Just crc -> [0x05] ++ crc }) ++
                 (if eh^.restrictionsPresent then
                    [0x01] ++
                    (error $ "No support for writing extended-header restriction flags yet!")
                  else [])

{- | /Extended Header Flags Meaning/

   The extended flags field, with its size described by 'number of flag
   bytes', is defined as:

@
     %0bcd0000
@

   Each flag that is set in the extended header has data attached, which
   comes in the order in which the flags are encountered (i.e. the data
   for flag 'b' comes before the data for flag 'c'). Unset flags cannot
   have any attached data. All unknown flags MUST be unset and their
   corresponding data removed when a tag is modified.

   Every set flag's data starts with a length byte, which contains a
   value between 0 and 128 ($00 - $7f), followed by data that has the
   field length indicated by the length byte. If a flag has no attached
   data, the value $00 is used as length byte.


 b - Tag is an update

     If this flag is set, the present tag is an update of a tag found
     earlier in the present file or stream. If frames defined as unique
     are found in the present tag, they are to override any
     corresponding ones found in the earlier tag. This flag has no
     corresponding data.

@
         Flag data length      $00
@


 c - CRC data present

     If this flag is set, a CRC-32 [ISO-3309] data is included in the
     extended header. The CRC is calculated on all the data between the
     header and footer as indicated by the header's tag length field,
     minus the extended header. Note that this includes the padding (if
     there is any), but excludes the footer. The CRC-32 is stored as an
     35 bit synchsafe integer, leaving the upper four bits always
     zeroed.

@
        Flag data length       $05
        Total frame CRC    5 * %0xxxxxxx
@


 d - Tag restrictions

     For some applications it might be desired to restrict a tag in more
     ways than imposed by the ID3v2 specification. Note that the
     presence of these restrictions does not affect how the tag is
     decoded, merely how it was restricted before encoding. If this flag
     is set the tag is restricted as follows:

@
        Flag data length       $01
        Restrictions           %ppqrrstt
@


     p - Tag size restrictions

@
       00   No more than 128 frames and 1 MB total tag size.
       01   No more than 64 frames and 128 KB total tag size.
       10   No more than 32 frames and 40 KB total tag size.
       11   No more than 32 frames and 4 KB total tag size.
@

     q - Text encoding restrictions

@
       0    No restrictions
       1    Strings are only encoded with ISO-8859-1 [ISO-8859-1] or
            UTF-8 [UTF-8].
@

     r - Text fields size restrictions

@
       00   No restrictions
       01   No string is longer than 1024 characters.
       10   No string is longer than 128 characters.
       11   No string is longer than 30 characters.
@

       Note that nothing is said about how many bytes is used to
       represent those characters, since it is encoding dependent. If a
       text frame consists of more than one string, the sum of the
       strungs is restricted as stated.

     s - Image encoding restrictions

@
       0   No restrictions
       1   Images are encoded only with PNG [PNG] or JPEG [JFIF].
@

     t - Image size restrictions

@
       00  No restrictions
       01  All images are 256x256 pixels or smaller.
       10  All images are 64x64 pixels or smaller.
       11  All images are exactly 64x64 pixels, unless required
           otherwise.
@
-}

{-
data ExtFlags = ExtFlags
                { extIsUpdate     :: Bool      -- ^ True if tag is an update
                , extCrc          :: [Word8]   -- ^ CRC data present
                , extRestrictions :: [Bool]    -- ^ Tag restrictions
                } deriving Eq

emptyExtFlags = ExtFlags False [] []
initExtFlags  = flip compose emptyExtFlags

isUpdate     = accessor extIsUpdate     (\x ef -> ef { extIsUpdate     = x})
crc          = accessor extCrc          (\x ef -> ef { extCrc          = x})
restrictions = accessor extRestrictions (\x ef -> ef { extRestrictions = x})

instance Show ExtFlags where
    show ef = "Extended Flags:\n"++
                (if ef^.isUpdate           then "\t- This tag is an update\n"          else "")++
                (if ef^.crc /= []          then "\t- There is some CRC data...\n"      else "")++
                (if ef^.restrictions /= [] then "\t- There are some restrictions...\n" else "")

instance Parsed ExtFlags where
    unparse ef = (if ef^.isUpdate           then [0x00]                                       else []) ++
                 (if ef^.crc /= []          then [0x05]++(ef^.crc)                            else []) ++
                 (if ef^.restrictions /= [] then [0x01]++(unparse $ Flags $ ef^.restrictions) else [])
-}
