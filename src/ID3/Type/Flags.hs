module ID3.Type.Flags (flagsToWord8) where

import Data.Bits (bit)
import Data.Word (Word8)

flagsToWord8 :: (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool) -> Word8
flagsToWord8 (b7, b6, b5, b4, b3, b2, b1, b0) = getBit 7 b7 + getBit 6 b6 + getBit 5 b5 +
                                                getBit 4 b4 + getBit 3 b3 + getBit 2 b2 +
                                                getBit 1 b1 + getBit 0 b0
  where getBit bitNum shouldSet = if shouldSet then bit bitNum else 0
