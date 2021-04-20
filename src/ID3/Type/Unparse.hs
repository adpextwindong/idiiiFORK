module ID3.Type.Unparse where

import ID3.Parser.UnSync        (synchronise)
import Codec.Binary.UTF8.String (encode, encodeString)
import Data.Word                (Word8)
import Data.Accessor

class Parsed a where
    unparse :: a -> [Word8]

instance Parsed Integer where
    unparse i = synchronise i

data Str = Str String deriving Eq
instance Parsed Str where
    unparse (Str s) = encode s

data Inf = Inf [(String, String)] deriving Eq
instance Show Inf where
    show (Inf xs) = concatMap (encodeString . snd) xs

instance Parsed Inf where
    unparse _ = undefined
    -- unparse (Inf xs) = 0x03 : (concat $ intersperse [0x00] $ map (\(_,s) -> unparse $ Str s) xs)

-----------------------------

class HasSize a where
    size :: Accessor a Integer
    updateSize :: a -> a
    updateSize x = size ^= (x^.size) $ x
