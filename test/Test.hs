import Utils

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Framework (Test, testGroup, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Control.Exception (catch)
import Data.Char (chr, isDigit, isHexDigit, isLower, isSpace, isUpper, ord)
import Data.Monoid (Monoid(..))
import Control.Monad
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
import qualified Pipes.Text as TP
import qualified Pipes.ByteString as BP 
import qualified Pipes as P 

main :: IO ()
main = defaultMain [tests]
-- >>> :main  -a 10000
tests = testGroup "stream_decode" [
  -- testProperty "t_utf8_incr_valid" t_utf8_incr_valid,
  testProperty "t_utf8_incr_mixed" t_utf8_incr_mixed ,
  testProperty "t_utf8_incr_pipe" t_utf8_incr_pipe,
  testProperty "t_utf8_dec_some" t_utf8_dec_some]

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
       let chunkSize = mod n 7 + 1
       forAll (vector 9) $ 
              (roundtrip . chunk chunkSize . appendBytes txt) `eq` (appendBytes txt)
    where 
    roundtrip :: [B.ByteString] -> B.ByteString
    roundtrip bss = go PE.streamDecodeUtf8 B.empty bss where    
       go dec acc [] = acc   
       go dec acc [bs]  = case dec bs of 
          PE.Some t l dec' -> acc <> E.encodeUtf8 t <> l
          PE.Other t bs'   -> acc <> E.encodeUtf8 t <> bs' 
       go dec acc (bs:bss) = case dec bs of 
         PE.Some t l dec' -> go dec' (acc <> E.encodeUtf8 t) bss
         PE.Other t bs'   -> acc <> E.encodeUtf8 t <> bs' <> B.concat bss
    chunk n bs = let (a,b) = B.splitAt n bs in if B.null a then [] else a : chunk n b
    appendBytes txt bts = E.encodeUtf8 txt <> B.pack bts ; (<>) = B.append

t_utf8_incr_pipe  = do    
       Positive  m <- arbitrary
       Positive n  <- arbitrary  
       txt         <- genUnicode
       let chunkSize = mod n 7 + 1
           bytesLength = mod 10 m
       forAll (vector bytesLength) $ 
              (BL.toStrict . BP.toLazy . roundtrip . P.each . chunk chunkSize . appendBytes txt) 
              `eq` 
              appendBytes txt
    where 
    roundtrip :: Monad m => P.Producer B.ByteString m r -> P.Producer B.ByteString m r
    roundtrip p = join (TP.decodeUtf8 p P.>-> TP.encodeUtf8) 
    chunk n bs = let (a,b) = B.splitAt n bs in if B.null a then [] else a : chunk n b
    appendBytes txt bts = E.encodeUtf8 txt <> B.pack bts ; (<>) = B.append

--
t_utf8_dec_some = do    
       Positive  m <- arbitrary
       txt         <- genUnicode
       let bytesLength = mod 10 m :: Int
       forAll (vector bytesLength) $ 
              (roundtrip . appendBytes txt) 
              `eq` 
              appendBytes txt
    where 
    roundtrip bs = case PE.decodeSomeUtf8 bs of
                        (txt,bys) -> E.encodeUtf8 txt <> bys
    appendBytes txt bts = E.encodeUtf8 txt <> B.pack bts ; (<>) = B.append




