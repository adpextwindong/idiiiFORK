module ID3.ReadTag (hReadTag, readTag) where

import Data.Accessor
import ID3.Parser (run)
import ID3.Parser.Tag
import ID3.Parser.Header
import ID3.Type (ID3Tag)
import ID3.Type.Header
import System.IO
import System.IO.Binary

hReadTag :: Handle -> IO (Maybe ID3Tag)
hReadTag hdl = do
    hSeek hdl AbsoluteSeek 0
    beginning <- hGetBufStr hdl 10
    case run parseHeader beginning of
         (Left  _     , _) -> return Nothing
         (Right header, _) -> do
             chunk <- hGetBufStr hdl $ fromInteger (header^.tagSize)
             case run (parseTag_ header) chunk of
                  (Left  _  , _) -> return  Nothing
                  (Right tag, _) -> return (Just tag)

readTag :: FilePath -> IO (Maybe ID3Tag)
readTag file = withBinaryFile file ReadWriteMode hReadTag
