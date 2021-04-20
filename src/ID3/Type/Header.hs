module ID3.Type.Header where

import ID3.Type.Unparse
import ID3.Type.Flags (flagsToWord8)
import Data.Word (Word8)
import Data.Accessor
import Data.Accessor.Basic (compose)

{- | /ID3v2 HEADER OVERVIEW/

   The first part of the ID3v2 tag is the 10 byte tag header, laid out
   as follows:

@
     ID3v2/file identifier      \"ID3\"
     ID3v2 version              $04 00
     ID3v2 flags                %abcd0000
     ID3v2 size             4 * %0xxxxxxx
@

   The first three bytes of the tag are always \"ID3\", to indicate that
   this is an ID3v2 tag, directly followed by the two version bytes. The
   first byte of ID3v2 version is its major version, while the second
   byte is its revision number. In this case this is ID3v2.4.0. All
   revisions are backwards compatible while major versions are not. If
   software with ID3v2.4.0 and below support should encounter version
   five or higher it should simply ignore the whole tag.
-}
data ID3Header = ID3Header
  { tagVersion_ :: TagVersion     -- ^ id3v2 version: @[major version, revision number]@
  , tagFlags_   :: TagFlags       -- ^ header flags as Bool values
  , tagSize_    :: TagSize        -- ^ full size of tag
  }   deriving Eq

emptyID3Header :: ID3Header
--emptyID3Header = ID3Header (4, 0) emptyFlags 0
emptyID3Header = ID3Header (4, 0) (TagFlags (False, False, False, False)) 0
initID3Header :: [ID3Header -> ID3Header] -> ID3Header
initID3Header  = flip compose emptyID3Header

tagVersion :: Accessor ID3Header TagVersion
tagVersion = accessor tagVersion_ (\v h -> h {tagVersion_ = v})
tagFlags :: Accessor ID3Header TagFlags
tagFlags   = accessor tagFlags_   (\f h -> h {tagFlags_   = f})
tagSize :: Accessor ID3Header TagSize
tagSize    = accessor tagSize_    (\s h -> h {tagSize_    = s})

instance Show ID3Header where
    show header = "ID3v2"++"."++(show v1)++"."++(show v2)++"\n"++
                                (showTagFlags $ header^.tagFlags)++
                  "Tag size: "++(show $ header^.tagSize   )++" bytes\n"
                  where (v1, v2) = header^.tagVersion

instance Parsed ID3Header where
    unparse h = (unparse $ Str "ID3") ++ [v1, v2] ++
                (unparse $ h^.tagFlags) ++ (unparse $ h^.tagSize)
      where (v1, v2) = h^.tagVersion

{- | id3v2 version
    @major version . revision number@
-}
type TagVersion = (Word8, Word8)


{- | /MEANING OF FLAGS/

@
    ID3v2 flags                %abcd0000
@

   The version is followed by the ID3v2 flags field, of which currently
    four flags are used:

   @a@ - /Unsynchronisation/

     Bit 7 in the 'ID3v2 flags' indicates whether or not
     unsynchronisation is applied on all frames (see section 6.1 for
     details); a set bit indicates usage.

   @b@ - /Extended header/

     The second bit (bit 6) indicates whether or not the header is
     followed by an extended header. The extended header is described in
     section 3.2. A set bit indicates the presence of an extended
     header.

   @c@ - /Experimental indicator/

     The third bit (bit 5) is used as an 'experimental indicator'. This
     flag SHALL always be set when the tag is in an experimental stage.

   @d@ - /Footer present/

     Bit 4 indicates that a footer (section 3.4) is present at the very
     end of the tag. A set bit indicates the presence of a footer.


   All the other flags MUST be cleared. If one of these undefined flags
   are set, the tag might not be readable for a parser that does not
   know the flags function.
-}

data TagFlags = TagFlags (Bool, Bool, Bool, Bool) deriving (Show, Eq)
unsynchFlag :: TagFlags -> Bool
unsynchFlag        (TagFlags (f, _, _, _)) = f
extendedHeaderFlag :: TagFlags -> Bool
extendedHeaderFlag (TagFlags (_, f, _, _)) = f
experimentalFlag :: TagFlags -> Bool
experimentalFlag   (TagFlags (_, _, f, _)) = f
footerFlag :: TagFlags -> Bool
footerFlag         (TagFlags (_, _, _, f)) = f
anyFlagsOn :: TagFlags -> Bool
anyFlagsOn         (TagFlags (a, b, c, d)) = a || b || c || d

showTagFlags :: TagFlags -> String
showTagFlags f = if not $ anyFlagsOn f then "" else
                   "Flags:\n" ++
                   (if unsynchFlag f        then "\t- all frames are unsynchronised\n"  else "") ++
                   (if extendedHeaderFlag f then "\t- tag has extended header\n"        else "") ++
                   (if experimentalFlag f   then "\t- tag is in experimental stage\n"   else "") ++
                   (if footerFlag f         then "\t- tag has footer at the very end\n" else "")

instance Parsed TagFlags where
    unparse (TagFlags (a, b, c, d)) = [flagsToWord8 (a, b, c, d, False, False, False, False)]

{- | /SIZE BYTES/

  The ID3v2 tag size is stored as a 32 bit synchsafe integer (section
   6.2), making a total of 28 effective bits (representing up to 256MB).

   The ID3v2 tag size is the sum of the byte length of the extended
   header, the padding and the frames after unsynchronisation. If a
   footer is present this equals to ('total size' - 20) bytes, otherwise
   ('total size' - 10) bytes.
-}
type TagSize = Integer
