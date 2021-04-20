{- | UNSYNCHRONISATION

    This module contents a couple of functions to convert readed bytes (as ['Word8']) of synchronized values to unsynchronised 'Integer'.


   /UNSYNCRONISATION/


   The only purpose of unsynchronisation is to make the ID3v2 tag as
   compatible as possible with existing software and hardware. There is
   no use in 'unsynchronising' tags if the file is only to be processed
   only by ID3v2 aware software and hardware. Unsynchronisation is only
   useful with tags in MPEG 1/2 layer I, II and III, MPEG 2.5 and AAC
   files.

 1. /The unsynchronisation scheme/

   Whenever a false synchronisation is found within the tag, one zeroed
   byte is inserted after the first false synchronisation byte. The
   format of synchronisations that should be altered by ID3 syncIntegerrs is
   as follows:

@
         %11111111 111xxxxx
@

   and should be replaced with:

@
         %11111111 00000000 111xxxxx
@

   This has the side effect that all $FF 00 combinations have to be
   altered, so they will not be affected by the decoding process.
   Therefore all the $FF 00 combinations have to be replaced with the
   $FF 00 00 combination during the unsynchronisation.

   To indicate usage of the unsynchronisation, the unsynchronisation
   flag in the frame header should be set. This bit MUST be set if the
   frame was altered by the unsynchronisation and SHOULD NOT be set if
   unaltered. If all frames in the tag are unsynchronised the
   unsynchronisation flag in the tag header SHOULD be set. It MUST NOT
   be set if the tag has a frame which is not unsynchronised.

   Assume the first byte of the audio to be $FF. The special case when
   the last byte of the last frame is $FF and no padding nor footer is
   used will then introduce a false synchronisation. This can be solved
   by adding a footer, adding padding or unsynchronising the frame and
   add $00 to the end of the frame data, thus adding more byte to the
   frame size than a normal unsynchronisation would. Although not
   preferred, it is allowed to apply the last method on all frames
   ending with $FF.

   It is preferred that the tag is either completely unsynchronised or
   not unsynchronised at all. A completely unsynchronised tag has no
   false synchonisations in it, as defined above, and does not end with
   $FF. A completely non-unsynchronised tag contains no unsynchronised
   frames, and thus the unsynchronisation flag in the header is cleared.

   Do bear in mind, that if compression or encryption is used, the
   unsynchronisation scheme MUST be applied afterwards. When decoding an
   unsynchronised frame, the unsynchronisation scheme MUST be reversed
   first, encryption and decompression afterwards.

 2. /Synchsafe integers/

   In some parts of the tag it is inconvenient to use the
   unsychronisation scheme because the size of unsynchronised data is
   not known in advance, which is particularly problematic with size
   descriptors. The solution in ID3v2 is to use synchsafe integers, in
   which there can never be any false synchs. Synchsafe integers are
   integers that keep its highest bit (bit 7) zeroed, making seven bits
   out of eight available. Thus a 32 bit synchsafe integer can store 28
   bits of information.

   Example:

@
     255 (%11111111) syncIntegerd as a 16 bit synchsafe integer is 383
     (%00000001 01111111).
@


(<http://www.id3.org/id3v2.4.0-structure>)
-}

module ID3.Parser.UnSync
    (
      wordsToInteger
    , unSyncInteger
    , unSynchronise
    , integerToWords
    , syncInteger
    , synchronise
    )
where

import Data.Bits
import Data.Word (Word8)

-- | converting list of bytes to 'Integer' value
wordsToInteger :: [Word8] -> Integer
wordsToInteger = sum . (zipWith (*) (iterate (*16^(2::Integer)) 1)) . reverse . map toInteger

-- | unsynchronisation between 'Integer's
unSyncInteger :: Integer -> Integer
unSyncInteger n = (n .&. 0x7f)
        .|.(n .&. 0x7f00)     `shiftR` 1
        .|.(n .&. 0x7f0000)   `shiftR` 2
        .|.(n .&. 0x7f000000) `shiftR` 3

-- | unsychronisation (just @unSyncInteger . wordsToInteger@)
unSynchronise :: [Word8] -> Integer
unSynchronise = unSyncInteger . wordsToInteger

---------------------------------------------------------------------------------------

-- | converting 'Integer' value to list of bytes
integerToWords :: Int -> Integer -> [Word8]
integerToWords n x = map (fromInteger . getByte x) $ reverse [0..(n-1)]
        where
            byte n0 = 2^(8*n0) * (2^(8::Integer) - 1)
            x0 `getByte` n0 = (x0 .&. (byte n0)) `shiftR` (8*n0)


-- | synchronisation between 'Integer's
syncInteger :: Integer -> Integer
syncInteger n = (n .&. 0x7f)
        .|.(n .&. 0x3f80)     `shiftL` 1
        .|.(n .&. 0x1fc000)   `shiftL` 2
        .|.(n .&. 0xfe00000)  `shiftL` 3

{-prop x = x == (wordsToInteger $ integerToWords 4 x)-}

-- | sychronisation (just @integerToWords 4 . syncInteger@)
synchronise :: Integer -> [Word8]
synchronise = integerToWords 4 . syncInteger
