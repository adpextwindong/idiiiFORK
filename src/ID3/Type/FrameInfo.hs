module ID3.Type.FrameInfo where

import ID3.Parser.UnSync        (integerToWords)
import ID3.Type.Unparse
import Codec.Binary.UTF8.String (encode)
import Data.Accessor
import Data.Word                (Word8)

data FrameInfo = UFID { owner :: String
                      , id    :: [Word8] }
               | Text { enc   :: Integer
                      , text  :: String }
               | TXXX { enc   :: Integer
                      , descr :: String
                      , text  :: String }
               | URL  { url   :: String }
               | WXXX { enc   :: Integer
                      , descr :: String
                      , url   :: String }
               | MCDI { tocData :: [Word8] }
               | ETCO -- { format :: Integer
                      -- , events :: [Event]} -- TODO
               | MLLT -- TODO
               | SYTC -- TODO
               | USLT { enc   :: Integer
                      , lang  :: String
                      , descr :: String
                      , text  :: String }
               | SYLT { enc     :: Integer
                      , lang    :: String
                      , timeFormat :: Integer
                      , content :: Integer
                      , descr   :: String } -- .... TODO
               | COMM { enc   :: Integer
                      , lang  :: String
                      , descr :: String
                      , text  :: String }
               | RVA2 -- TODO
               | EQU2 -- TODO
               | RVRB -- TODO
               | APIC { enc :: Integer
                      , mime :: String
                      , picType :: Word8
                      , descr   :: String
                      , picData :: [Word8] } -- .... TODO
               | GEOB -- TODO
               | PCNT { counter :: Integer }
               | POPM { email   :: String
                      , rating  :: Integer
                      , counter :: Integer }
               | RBUF -- TODO
               | AENC -- TODO
               | LINK -- TODO
               | POSS -- TODO
               | USER { enc   :: Integer
                      , lang  :: String
                      , text  :: String }
               | OWNE -- TODO
               | COMR -- TODO
               | ENCR -- TODO
               | GRID -- TODO
               | PRIV { ownerId     :: String
                      , privateData :: [Word8] }
               | SIGN -- TODO
               | ASPI -- TODO

               -- not native
               | TCMP { isPart :: Bool }
               | Unknown { frameData :: [Word8] }
          deriving (Eq, Show)


encodeAll :: [String] -> [Word8]
encodeAll = concatMap encode

infoTextContent :: Accessor FrameInfo String
infoTextContent = accessor text (\x f -> f {text = x})
--url = accessor (encode . url) (\x f -> f {url = decode x})

instance Parsed FrameInfo where
    unparse inf = case inf of
               UFID owner0 theid0    -> (encode owner0) ++ [0x00] ++ theid0
               Text enc0 text0       -> (fromInteger enc0) : (encode text0)
               TXXX enc0 descr0 text0 -> (fromInteger enc0) : (encode descr0) ++ [0x00] ++ (encode text0)
               URL  url0            ->  encode url0
               WXXX enc0 descr0 url0  -> (fromInteger enc0) : (encode descr0) ++ [0x00] ++ (encode url0)
               MCDI tocData0        -> tocData0
               --ETCO -- { format :: Integer
                    -- , events :: [Event]} -- TODO
               --MLLT -- TODO
               --SYTC -- TODO
               USLT enc0 lang0 descr0 text0 -> (fromInteger enc0) : (encodeAll [lang0, descr0]) ++ [0x00] ++ (encode text0)
               --SYLT enc lang timeFormat content descr
               COMM enc0 lang0 descr0 text0 -> (fromInteger enc0) : (encodeAll [lang0, descr0]) ++ [0x00] ++ (encode text0)
               --RVA2 -- TODO
               --EQU2 -- TODO
               --RVRB -- TODO
               APIC enc0 mime0 picType0 descr0 picData0 -> (fromInteger enc0) : (encode mime0) ++ [0x00,picType0] ++ (encode descr0) ++[0x00]++ picData0
               --GEOB -- TODO
               PCNT counter0              -> integerToWords 4 counter0
               POPM email0 rating0 counter0 -> (encode email0) ++ [0x00, fromInteger rating0] ++ (integerToWords 4 counter0)
               --RBUF -- TODO
               --AENC -- TODO
               --LINK -- TODO
               --POSS -- TODO
               USER enc0 lang0 text0 -> (fromInteger enc0) : (encode lang0) ++ [0x00] ++ (encode text0)
               --OWNE -- TODO
               --COMR -- TODO
               --ENCR -- TODO
               --GRID -- TODO
               PRIV ownerId0 privateData0 -> (encode ownerId0) ++ privateData0
               --SIGN -- TODO
               --ASPI -- TODO
               TCMP isPart0 -> 0x03 : (encode $ if isPart0 then "1" else "0")
               Unknown x -> x
               f -> error $ "No pattern matched for encoding following frame: " ++ show f
