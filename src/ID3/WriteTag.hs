module ID3.WriteTag
( hWriteTag
, writeTag
) where

import ID3.Type
import ID3.ReadTag
import Data.Accessor
import System.IO
import System.IO.Binary
--import Data.Word (Word8)

hWriteTag :: ID3Tag -> Handle -> IO ()
hWriteTag newTag hdl = do
    smth <- hReadTag hdl
    case smth of
         Nothing     -> insertTag (padding^=pad $ newTag) 0
         Just oldTag ->
                let diff = (getFullSize oldTag) - (getActualSize newTag)
                in  if diff <= 0
                       then insertTag (padding^=pad $ newTag) $ 10 + (oldTag^.size)
                       else do
                           hSeek hdl AbsoluteSeek 0
                           hPutBufStr hdl (unparse (padding^=diff $ newTag))
                           --rest <- readFrom $ 10+(oldTag^.size) :: IO [Word8]
                           --withBinaryFile "_m.mp3" WriteMode $ \h -> hPutBufStr h rest
    where
        pad = 4096      -- default padding size
        insertTag t pos = do
            hSeek hdl AbsoluteSeek pos
            rest <- hGetBufStr hdl . fromInteger =<< hFileSize hdl
            --withBinaryFile "_m.mp3" WriteMode $ \h -> hPutBufStr h rest
            hSeek hdl AbsoluteSeek 0
            hPutBufStr hdl (unparse t ++ rest)

writeTag ::  FilePath -> ID3Tag -> IO ()
writeTag file = withBinaryFile file ReadWriteMode . hWriteTag
