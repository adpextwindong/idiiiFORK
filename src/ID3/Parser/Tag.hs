module ID3.Parser.Tag (parseTag, parseTag_) where

{--  IMPORTS -}
import Data.Accessor

import ID3.Parser.Header
import ID3.Parser.ExtHeader
import ID3.Parser.Frame
import ID3.Parser.General

import ID3.Type
-- --}

parseTag :: TagParser ID3Tag
parseTag = do
    h <- parseHeader                                             `err` "header"
    parseTag_ h

parseTag_ :: ID3Header -> TagParser ID3Tag
parseTag_ h = do
    tagVersionSet (h^.tagVersion)
    flagsSet (h^.tagFlags)
    ext <- if (extendedHeaderFlag $ h^.tagFlags)
             then parseExtHeader >>= return . Just
             else return Nothing                                `err` "ext header"
    (idList, fs)  <- parseFrames                                `err` "frames"
    _ <- if (footerFlag $ h^.tagFlags) then parseFooter else return h     `err` "footer"
    return $ initID3Tag [header^=h, extHeader^=ext, frames^=fs, framesOrder^=idList, padding^=(h^.tagSize) - (framesSize fs)]
