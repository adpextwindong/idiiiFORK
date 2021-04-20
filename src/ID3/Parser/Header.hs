module ID3.Parser.Header (parseHeader, parseFooter) where

----  IMPORTS
import Text.ParserCombinators.Poly.State
import ID3.Parser.General
import ID3.Type.Header
import Data.Accessor
import Data.Bits (testBit)
----


---,-----------------------------
-- | Parses id3v2 'Header'
parseHeader :: TagParser ID3Header
parseHeader = do
    _ <- string "ID3"              `err` "ID3"
    parseHeader_

parseHeader_ :: Parser St Token ID3Header
parseHeader_ = do
    v <- parseVersion         `err` "tag version"
    f <- parseFlags           `err` "tag flags"
    s <- parseTagSize         `err` "tag size"
    return $ initID3Header [tagVersion^=v, tagFlags^=f, tagSize^=s]

parseVersion :: TagParser TagVersion
parseVersion = do
    v1 <- anyWord8
    v2 <- anyWord8
    return $ (v1, v2)

parseFlags :: TagParser TagFlags
parseFlags = do
    flags <- anyWord8
    let bit = testBit flags
    return $ TagFlags (bit 7, bit 6, bit 5, bit 4)

parseTagSize :: TagParser TagSize
parseTagSize = parseSize 4 True

{- | /ID3v2 FOOTER/ (optional)

   To speed up the process of locating an ID3v2 tag when searching from
   the end of a file, a footer can be added to the tag. It is REQUIRED
   to add a footer to an appended tag, i.e. a tag located after all
   audio data. The footer is a copy of the header, but with a different
   identifier.

@
     ID3v2 identifier           \"3DI\"
     ID3v2 version              $04 00
     ID3v2 flags                %abcd0000
     ID3v2 size             4 * %0xxxxxxx
@

-}
parseFooter :: TagParser ID3Header
parseFooter = do
    _ <- string "3DI"
    parseHeader_
