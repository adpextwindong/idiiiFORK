-- | This module provides parsers for Extended Header
--
-- (<http://www.id3.org/id3v2.4.0-structure>)
module ID3.Parser.ExtHeader
    (parseExtHeader)
where

----  IMPORTS
import ID3.Parser.General
import ID3.Type.ExtHeader

import Data.Accessor
import Data.Bits (testBit)
import Data.Word (Word8)
----


-- | Parses Extended Header as 'ID3ExtHeader' structure
parseExtHeader :: TagParser ID3ExtHeader
parseExtHeader = do
    s <- parseSize 4 True `err` "ext header size"      -- Extended header size: 4 * %0xxxxxxx
    _ <- word8 0x01 `err` "ext header pre-flags"       -- Number of flag bytes: $01
    flags <- anyWord8 `err` "ext header flags"         -- Extended Flags:       %0bcd0000
    let (b, c, d) = (testBit flags 6, testBit flags 5, testBit flags 4)
    bb <- if b then parseB else return False
    cc <- if c
          then do
            crc <- parseC
            return $ Just crc
          else return Nothing
    dd <- if d then parseD else return False
    return $ initID3ExtHeader [ extSize^=s
                              , isUpdate^=bb
                              , crcData^=cc
                              , restrictionsPresent^=dd
                              ]


-- | parser for @b@ flag - 'isUpdate'
parseB :: TagParser Bool
parseB = do
    _ <- word8 0x00
    return True

-- | parser for @c@ flag - CRC data
parseC :: TagParser [Word8]
parseC = do
    _ <- word8 0x05
    count (5::Integer) anyWord8

-- | parser for @d@ flag - restrictions flags
-- TODO: Properly read these flags into a data structure...
parseD :: TagParser Bool
parseD = do
    _ <- word8 0x01
    _ <- anyWord8
    return True
