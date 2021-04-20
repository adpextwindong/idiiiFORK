module ID3.Type.Tag
where

import Data.List            ((\\))
import Data.Maybe
import Data.Map             (Map)
import qualified Data.Map as Map
import Data.Accessor
import Data.Accessor.Basic  (compose)

import ID3.Type.Header
import ID3.Type.ExtHeader
import ID3.Type.Frame
import ID3.Type.Unparse

{-- /ID3v2 Format Overview/

   ID3v2 is a general tagging format for audio, which makes it possible
   to store meta data about the audio inside the audio file itself. The
   ID3 tag described in this document is mainly targeted at files
   encoded with MPEG-1/2 layer I, MPEG-1/2 layer II, MPEG-1/2 layer III
   and MPEG-2.5, but may work with other types of encoded audio or as a
   stand alone format for audio meta data.

   ID3v2 is designed to be as flexible and expandable as possible to
   meet new meta information needs that might arise. To achieve that
   ID3v2 is constructed as a container for several information blocks,
   called frames, whose format need not be known to the software that
   encounters them. At the start of every frame is an unique and
   predefined identifier, a size descriptor that allows software to skip
   unknown frames and a flags field. The flags describes encoding
   details and if the frame should remain in the tag, should it be
   unknown to the software, if the file is altered.

   The bitorder in ID3v2 is most significant bit first (MSB). The
   byteorder in multibyte numbers is most significant byte first (e.g.
   $12345678 would be encoded $12 34 56 78), also known as big endian
   and network byte order.

   Overall tag structure:

     +-----------------------------+
     |      Header (10 bytes)      |
     +-----------------------------+
     |       Extended Header       |
     | (variable length, OPTIONAL) |
     +-----------------------------+
     |   Frames (variable length)  |
     +-----------------------------+
     |           Padding           |
     | (variable length, OPTIONAL) |
     +-----------------------------+
     | Footer (10 bytes, OPTIONAL) |
     +-----------------------------+

   In general, padding and footer are mutually exclusive.
--}

data ID3Tag = ID3Tag
              { tagHeader    :: ID3Header
              , tagExtHeader :: Maybe ID3ExtHeader
              , tagFrames    :: Map FrameID ID3Frame
              , tagFramesOrder :: [FrameID]
              , tagPadding   :: Integer
              } deriving Eq

emptyID3Tag :: ID3Tag
emptyID3Tag = ID3Tag emptyID3Header Nothing Map.empty [] 0
initID3Tag :: [ID3Tag -> ID3Tag] -> ID3Tag
initID3Tag  = flip compose emptyID3Tag

header :: Accessor ID3Tag ID3Header
header   = accessor tagHeader     (\x t -> t {tagHeader    = x})
version :: Accessor ID3Tag TagVersion
version  = header .> tagVersion

---------------------
instance HasSize ID3Tag where
    size = accessor getFullSize setSize

setSize :: TagSize -> ID3Tag -> ID3Tag
setSize = setVal $ header .> tagSize
getFullSize :: ID3Tag -> FrameSize
getFullSize t = getActualSize t + (t^.padding)

getActualSize :: ID3Tag -> FrameSize
getActualSize t = (footerSize t) + (framesSize (t^.frames)) + (extHSize t)

framesSize :: Map FrameID ID3Frame -> FrameSize
framesSize fs = Map.foldr (\fr x -> fr^.frHeader^.frSize + 10 + x) 0 fs
footerSize :: ID3Tag -> Integer
footerSize t = if (footerFlag $ t^.flags) then 10 else 0
extHSize :: ID3Tag -> Integer
extHSize t = case t^.extHeader of
                  Just eH -> eH^.extSize
                  Nothing -> 0
padding :: Accessor ID3Tag Integer
padding = accessor tagPadding (\x t -> t {tagPadding = x})

---------------------
flags :: Accessor ID3Tag TagFlags
flags    = header .> tagFlags

extHeader :: Accessor ID3Tag (Maybe ID3ExtHeader)
extHeader = accessor tagExtHeader (\x t -> t {tagExtHeader = x})
frames :: Accessor ID3Tag (Map FrameID ID3Frame)
frames    = accessor tagFrames    (\x t -> t {tagFrames    = x})

framesOrder :: Accessor ID3Tag [FrameID]
framesOrder = accessor tagFramesOrder (\x t -> t {tagFramesOrder = x})

frame :: FrameID -> Accessor ID3Tag (Maybe ID3Frame)
frame frid = accessor (\t -> getFrame t frid) (\x t -> setFrame t frid x)

getFrame :: ID3Tag -> FrameID -> Maybe ID3Frame
getFrame t f = Map.lookup f (t^.frames)
setFrame :: ID3Tag -> FrameID -> Maybe ID3Frame -> ID3Tag
setFrame t f x = updateSize $ frames ^= Map.alter (\_ -> x) f (t^.frames) $ t

---------------------------------------------------------

sortFrames :: Map FrameID ID3Frame -> [FrameID] -> [ID3Frame]
sortFrames fs ids = mapMaybe (\frid -> Map.lookup frid fs) $ ids ++ (Map.keys fs \\ ids)

instance Show ID3Tag where
    show t = ( show $ t^.header)                               ++"\n"++
             ( "Full   tag size: "++(show $ getFullSize   t))  ++"\n"++
             ( "Actual tag size: "++(show $ getActualSize t))  ++"\n"++
             ( "Padding    size: "++(show $ t^.padding))       ++"\n"++
             ( unwords $ t^.framesOrder )                      ++"\n"++
             ( maybe "" show $ t^.extHeader )                  ++"\n"++
             ( concatMap show $ sortFrames (t^.frames) (t^.framesOrder) )

instance Parsed ID3Tag where
    unparse t = ( unparse $ t^.header ) ++
                ( maybe [] unparse $ t^.extHeader ) ++
                ( concatMap unparse $ sortFrames (t^.frames) (t^.framesOrder) ) ++
                ( if (footerFlag $ t^.flags) then unparseFooter ++ (drop 10 unparsePadding) else unparsePadding )
                where
                    unparseFooter = (unparse $ Str "3DI") ++ (drop 3 $ unparse $ t^.header)
                    unparsePadding = replicate (fromInteger $ t^.padding) 0x00
