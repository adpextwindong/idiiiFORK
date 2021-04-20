-- | This module contain different general parsers.
module ID3.Parser.General where

----  IMPORTS
import Text.ParserCombinators.Poly.State (Parser, runParser, stGet, stUpdate, onFail, next,
                                          satisfy, adjustErr)
import ID3.Parser.UnSync
import ID3.Type.Header (TagVersion, TagFlags(..))
--import qualified Data.ByteString.Lazy as BS
--import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Word (Word8)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf16LE, decodeUtf16BE, decodeUtf8)
import Data.Char (chr)
import Control.Monad (when)
----

---,--------------------------------
-- | Just a synonim for one item of input stream
type Token = Word8

-- | Parsers state
data St = State { id3TagVersion  :: TagVersion
                , headerFlags    :: TagFlags -- ^ flags from tag's header
                , tagPos         :: Integer  -- ^ current position in tag
--              , frFlags        :: [Bool]}  -- ^ current frame flags
                , curSize        :: Integer  -- ^ current frame size
                }

instance Show St where
    show st = show (tagPos st, curSize st)
initState :: St
initState = State (4, 0) (TagFlags (False, False, False, False)) 0 10

type CharEncoding = Integer

type TagParser = Parser St Token

run :: TagParser a -> [Word8] -> (Either String a, [Token])
run p cont = (result, rest) where (result, _, rest) = runParser p initState cont


---,--------------------------------
tagVersionGet :: TagParser TagVersion
tagVersionGet = stGet >>= return . id3TagVersion

tagVersionSet :: TagVersion -> TagParser ()
tagVersionSet v = stUpdate (\st -> st {id3TagVersion = v})

---,--------------------------------
flagsGet :: TagParser TagFlags
flagsGet = stGet >>= return . headerFlags

flagsSet :: TagFlags -> TagParser ()
flagsSet fs = stUpdate (\st -> st {headerFlags = fs})

---,--------------------------------

-- pos = accessor (stGet >>= return . tagPos) (stUpdate $ \p st -> st {tagPos = p} )

-- | Returns 'tagPos' from 'St'.
posGet      :: TagParser Integer
posGet       = stGet >>= return . tagPos
-- | Updates 'tagPos' with given function.
posUpdate   :: (Integer -> Integer) -> TagParser ()
posUpdate f  = stUpdate ( \st -> st {tagPos = f (tagPos st)} )
-- | Sets 'tagPos' with given value.
posSet      :: Integer -> TagParser ()
posSet p     = posUpdate (\_ -> p)
-- | Decrements 'tagPos'.
posDec      :: TagParser ()
posDec       = posUpdate (\p -> p-1)
-- | Incremets 'tagPos'.
posInc      :: TagParser ()
posInc       = posUpdate (\p -> p+1)

---,--------------------------------
-- | Returns 'curSize' from 'St'.
sizeGet     :: TagParser Integer
sizeGet      = stGet >>= return . curSize
-- | Updates 'curSize' with given function.
sizeUpdate  :: (Integer -> Integer) -> TagParser ()
sizeUpdate f = stUpdate ( \st -> st {curSize = f (curSize st)} )
-- | Sets 'curSize' with given value.
sizeSet     :: Integer -> TagParser ()
sizeSet s    = sizeUpdate (\_ -> s)
-- | Decrements 'curSize'.
sizeDec     :: TagParser ()
sizeDec      = sizeUpdate (\x -> x-1)
-- | Incremets 'curSize'.
sizeInc     :: TagParser ()
sizeInc      = sizeUpdate (\x -> x+1)

---,--------------------------------
-- | Wrapper for /reiterative/ parsers.
--   Mnemonic: @if 'curSize' > 0 then@ continue @else@ stop
ifSize :: TagParser [a] -> TagParser [a]
ifSize p = do
    s <- sizeGet
    if s > 0
       then p
       else return []

-- | Wrapper for atomic parsers.
--   Increases 'tagPos' and decreases 'curSize'.
withSize :: TagParser b -> TagParser b
withSize p = do
    x <- p
    sizeDec
    posInc
    return x

--  Wrapper for any parser, that increaser tagPos and decreases curSize according to result of parsing
--   It is useful only for parsers, that return as much tokens, as they've read from the stream.
--   If result is list, then state updates according to it's length, else (!) result takes as a single-sized value.
{-withState :: TagParser a -> TagParser a
withState parser = do
        x <- parser
        case x of
            []    -> return x           -- empty list
            (_:_) -> do                 -- non-empty list
                    let n = toInteger $ length x
                    posUpdate (+n)
                    sizeUpdate (subtract n)
                    return x
            _     -> do                 -- singleton
                    posInc
                    sizeDec
                    return x
-}

---,--------------------------------
-- | @'many'' p@ parses a list of elements with individual parser @p@.
--   Cannot fail, since an empty list is a valid return value.
--   Unlike default 'many', stops if 'curSize' became 0.
many' :: TagParser a -> TagParser [a]
many' p = many1' p `onFail` return []

-- | Parse a non-empty list of items.
many1' :: TagParser a -> TagParser [a]
many1' p = ifSize $ do
    x <- p
    xs <- many' p
    return (x:xs)


---,--------------------------------
-- | @'manyTill'' p end@ parses a possibly-empty sequence of @p@'s, terminated by a @end@.
manyTill' :: TagParser a -> TagParser z -> TagParser [a]
manyTill' p end = manyTill1' p end `onFail` return []

-- | 'manyTill1\' p end' parses a non-empty sequence of p's, terminated by a end.
manyTill1' :: TagParser a -> TagParser z -> TagParser [a]
manyTill1' p end = ifSize $ (end >> return []) `onFail`
    (ifSize $ do
        x <- p
        xs <- manyTill' p end
        return (x:xs))

---,--------------------------------
-- | Parse a list of items separated by discarded junk.
sepBy' :: TagParser a -> TagParser sep -> TagParser [a]
sepBy' p sep = sepBy1' p sep `onFail` return []

-- | Parse a non-empty list of items separated by discarded junk.
sepBy1' :: TagParser a -> TagParser sep -> TagParser [a]
sepBy1' p sep= ifSize $ do
    x <- p
    xs <- many' (sep >> p)
    return (x:xs)

---,--------------------------------
-- | 'count n p' parses a precise number of items, n, using the parser p, in sequence.
count :: (Num n, Eq n) => n -> TagParser a -> TagParser [a]
count 0 _ = return []
count n p = do
    x <- p
    xs <- count (n-1) p
    return (x:xs)

-- | 'count' n p' parses a precise number of items, n, using the parser p, in sequence.
count' :: (Num n, Eq n) => n -> TagParser a -> TagParser [a]
count' 0 _ = return []
count' n p = ifSize $ do
    x <- p
    xs <- count' (n-1) p
    return (x:xs)

---,--------------------------------
-- | Hybrid of 'count' and 'sepBy\''
countSepBy' :: (Num n, Eq n) => n -> TagParser a -> TagParser sep -> TagParser [a]
countSepBy' 0 _ _ = return []
countSepBy' n p sep = ifSize $ do
    x <- p
    xs <- count' (n-1) (sep >> p)
    return (x:xs)

-- decodeLatin1 is due to be merged into Data.Text.Encoding
-- https://github.com/hvr/text/commit/c1c338717e4c278b54d940ec00df926f83ec3643
-- http://stackoverflow.com/questions/7544919/convert-between-latin1-encoded-data-bytestring-and-data-text
-- In the meantime we just hack it.

encPack :: CharEncoding -> [Token] -> String
encPack 0x00            s  = map (chr . fromIntegral) s
encPack 0x01 (0xFF:0xFE:s) = Text.unpack $ decodeUtf16LE $ BS.pack s
encPack 0x01 (0xFE:0xFF:s) = Text.unpack $ decodeUtf16BE $ BS.pack s
encPack 0x02            s  = Text.unpack $ decodeUtf16BE $ BS.pack s
encPack _               s  = Text.unpack $ decodeUtf8    $ BS.pack s

-- | Parses one value (as [Token]) till termination symbol
parseUntilWord8Null :: TagParser [Token]
parseUntilWord8Null = nonNull `manyTill'` (word8 0x00)

parseUntilWord16Null :: TagParser [Token]
parseUntilWord16Null = do
  s <- sizeGet
  when (s == 1) $ fail "Non-even number of bytes for UTF-16 string"
  if s > 1
    then do
      byte1 <- anyWord8
      byte2 <- anyWord8
      if byte1 == 0x00 && byte2 == 0x00
        then return []
        else do
          rest <- parseUntilWord16Null
          return $ [byte1, byte2] ++ rest
    else return []

-- any byte except null
nonNull :: Parser St Token Token
nonNull = withSize $ satisfy (/=0x00) `adjustErr` (++"\nWTF: nonNull")

-- | Parses a character-encoding "code", a one-byte value that should be 0, 1, 2, or 3
parseEncoding :: TagParser CharEncoding
parseEncoding = anyWord8 >>= (return . toInteger)

-- | Parses one value and returns it as a 'String'
parseString :: CharEncoding -> TagParser String
parseString enc = do
    v <- case enc of
      0x01 -> parseUntilWord16Null -- UTF-16
      0x02 -> parseUntilWord16Null -- UTF-16 BOM
      _    -> parseUntilWord8Null  -- ISO-8859-1 or UTF-8
    return $ encPack enc v

-- | Parses one value and returns it as a 'Integer'
parseNumber :: TagParser Integer
parseNumber = parseUntilWord8Null >>= return . sum . (zipWith (*) (iterate (*10) 1)) .
  reverse . map toInteger

-- | Parses 3 bytes of language value (as a String) and returns a pair ("Language", value)
parseLanguage :: TagParser String
parseLanguage = do
    lang <- count' (3 :: Integer) anyWord8
    return $ encPack 0x03 lang

--(<|>) = onFail

---,--------------------------------
-- | Takes a list of 'Parser's and applies them by turns.
parsers :: [TagParser a] -> TagParser [a]
parsers [] = return []
parsers (p:ps) = do
    x <- p
    xs <- parsers ps
    return (x:xs)

---,--------------------------------
-- | Parses given 'Token'.
word8 :: Token -> TagParser Token
word8 w = (withSize $ satisfy (==w)) `err` (" \nWTF: word8 "++(show w))

-- | Parses given list of 'Token's.
word8s :: [Token] -> TagParser [Token]
word8s ws = parsers $ map word8 ws

-- | Parses given 'ByteString'.
byteString :: BS.ByteString -> TagParser BS.ByteString
byteString bs = (word8s $ BS.unpack bs) >> return bs

-- | Same as 'byteString' but argument is simple 'String'.
string :: String -> TagParser BS.ByteString
string = byteString . C.pack

-- | Parses upper-case letters (as 'Token')
upper :: TagParser Token
upper = satisfy (\x -> (0x41<=x)&&(x<=0x5a)) `err` ("\nWTF: upper")

-- | Parses digit-symbol (as 'Token')
digit :: TagParser Token
digit = satisfy (\x -> (0x30<=x)&&(x<=0x39)) `err` ("\nWTF: digit")

---,--------------------------------
-- | Parses any 'Token'.
anyWord8 :: TagParser Token
anyWord8 = withSize next `err` "anyWord8"

err :: TagParser t -> String -> TagParser t
err p s = do
    pos <- posGet
    p `adjustErr` (++"\n"++"at "++(show pos)++": "++s)

---,--------------------------------
type Size = Integer
-- | 'parseSize n unsynchDecode' parses n bytes, doing decoding of "unsynchronized" data when unsynchDecode is True, and returns the represented 'Integer' value.
parseSize :: Integer -> Bool -> TagParser Size
parseSize n unsynchDecode = do
    s <- count n next
    posUpdate (+n)
    let size = if unsynchDecode then unSynchronise s else wordsToInteger s
    sizeSet size
    return size
