import Utils

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Framework (Test, testGroup, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Control.Exception (catch)
import Data.Char (chr, isDigit, isHexDigit, isLower, isSpace, isUpper, ord)
import Data.Monoid (Monoid(..))
import Data.String (fromString)
import Data.Text.Encoding.Error
import qualified Data.List as L

import qualified Data.Bits as Bits (shiftL, shiftR)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as E
import qualified Pipes.Text.Internal as PE

main :: IO ()
main = defaultMain [tests]
-- >>> :main  -a 10000

tests = testGroup "stream_decode" [

  testProperty "t_utf8_incr_valid" t_utf8_incr_valid,
  testProperty "t_utf8_incr_mixed" t_utf8_incr_mixed]

t_utf8_incr_valid  = do
        Positive n <- arbitrary
        forAll genUnicode $ recode n `eq` id
    where recode n = T.concat . feedChunksOf n PE.streamDecodeUtf8 . E.encodeUtf8
          feedChunksOf :: Int -> (B.ByteString -> PE.Decoding) -> B.ByteString
                       -> [T.Text]
          feedChunksOf n f bs
            | B.null bs  = []
            | otherwise  = let (a,b) = B.splitAt n bs
                               PE.Some t _ f' = f a
                           in case f a of 
                              PE.Some t _ f' -> t : feedChunksOf n f' b
                              _             -> []

t_utf8_incr_mixed  = do    
       Positive n <- arbitrary  
       txt <- genUnicode
       forAll (vector 9) $ (roundtrip . chunk (mod n 7 + 1) . appendBytes txt) `eq` appendBytes txt
    where 
    roundtrip :: [B.ByteString] -> B.ByteString
    roundtrip bss = go (PE.streamDecodeUtf8With Nothing) B.empty B.empty bss where                                                      
       go dec acc old [] = acc <> old
       go dec acc old (bs:bss) = case dec bs of 
         PE.Some t new dec' -> if T.null t then go dec' (acc <> E.encodeUtf8 t) (old <> new) bss
                                           else go dec' (acc <> E.encodeUtf8 t) new bss
         PE.Other t bs' -> if T.null t then acc <> old <> bs <> B.concat bss 
                                       else acc <> E.encodeUtf8 t <> bs' <> B.concat bss 
    chunk n bs = let (a,b) = B.splitAt n bs in if B.null a then [] else a : chunk n b
    appendBytes txt bts = E.encodeUtf8 txt <> B.pack bts ; (<>) = B.append
