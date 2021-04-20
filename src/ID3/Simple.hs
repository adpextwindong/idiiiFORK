module ID3.Simple
( Tag

, setArtist
, setTitle
, setAlbum
, setYear
, setTrack

, getArtist
, getTitle
, getAlbum
, getYear
, getTrack

, readTag
, writeTag

) where

import Data.Accessor
import Data.Maybe
import ID3.Type
import ID3.ReadTag
import ID3.WriteTag

type Tag = ID3Tag

getFrameText :: FrameID -> Tag -> Maybe String
getFrameText frid tag = case tag^.frame frid of
                           Nothing -> Nothing
                           Just fr -> Just (fr^.textContent)

setFrameText :: FrameID -> String -> Tag -> Tag
setFrameText frid x tag = edit $ fromMaybe (initFrame frid) (tag^.frame frid)
                  where edit f = (frame frid) ^= Just (updateSize $ textContent^=x $ f ) $ tag

-----------------------------------

getArtist :: Tag -> Maybe String
getArtist = getFrameText "TPE1"

setArtist :: String -> Tag -> Tag
setArtist = setFrameText "TPE1"

--artist = accessor getArtist setArtist
-----------------------------------

getTitle :: Tag -> Maybe String
getTitle = getFrameText "TIT2"

setTitle :: String -> Tag -> Tag
setTitle = setFrameText "TIT2"
-----------------------------------

getAlbum :: Tag -> Maybe String
getAlbum = getFrameText "TALB"

setAlbum :: String -> Tag -> Tag
setAlbum = setFrameText "TALB"
-----------------------------------

getYear :: Tag -> Maybe String
getYear = getFrameText "TDRC"

setYear :: String -> Tag -> Tag
setYear = setFrameText "TDRC"
-----------------------------------

getTrack :: Tag -> Maybe String
getTrack = getFrameText "TRCK"

setTrack :: String -> Tag -> Tag
setTrack = setFrameText "TRCK"
-----------------------------------
{-
get:: Tag -> Maybe String
get= getFrameText ""

set:: String -> Tag -> Tag
set= setFrameText ""
-----------------------------------
-}
