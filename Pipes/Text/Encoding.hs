{-# LANGUAGE RankNTypes, BangPatterns #-}
-- |

-- This module uses the stream decoding functions from the text-stream-decoding package
-- to define decoding functions and lenses.

module Pipes.Text.Encoding
    ( 
    -- * Lens type
    -- $lenses
    Codec
    -- * Standard lenses for viewing Text in ByteString
    -- $codecs
    , utf8
    , utf8Pure
    , utf16LE
    , utf16BE
    , utf32LE
    , utf32BE
    -- * Non-lens decoding functions 
    , decodeUtf8
    , decodeUtf8Pure
    , decodeUtf16LE
    , decodeUtf16BE
    , decodeUtf32LE
    , decodeUtf32BE
    -- * Functions for latin and ascii text
    -- $ascii
    , encodeAscii
    , decodeAscii
    , encodeIso8859_1
    , decodeIso8859_1
    ) 
    where

import Data.Char (ord)
import Data.ByteString as B 
import Data.ByteString (ByteString)
import Data.ByteString.Char8 as B8
import Data.Text (Text)
import qualified Data.Text as T 
import qualified Data.Text.Encoding as TE 
import Data.Text.StreamDecoding
import Control.Monad (join)
import Data.Word (Word8)
import Pipes


type Lens' a b = forall f . Functor f => (b -> f b) -> (a -> f a)

{- $lenses
    The 'Codec' type is just an aliased standard Prelude type. It just specializes 
    the @Lens'@ type synonymn used by the standard lens libraries, @lens@ and 
    @lens-families@ . You use them with
    the @view@ or @(^.)@ and @zoom@ functions from those libraries.
    
    Each codec lens looks into a byte stream that is understood to contain text.
    The stream of text it 'sees' in the stream of bytes begins at its head; it ends 
    by reverting to (returning) the original byte stream 
    beginning at the point of decoding failure. Where there is no decoding failure, 
    it returns an empty byte stream with its return value. 
    -}

type Codec
    =  forall m r
    .  Monad m
    => Lens' (Producer ByteString m r)
             (Producer Text m (Producer ByteString m r))

decodeStream :: Monad m 
       => (B.ByteString -> DecodeResult) 
       -> Producer ByteString m r -> Producer Text m (Producer ByteString m r)
decodeStream = loop where
  loop dec0 p = 
    do x <- lift (next p) 
       case x of Left r -> return (return r)
                 Right (chunk, p') -> case dec0 chunk of 
                    DecodeResultSuccess text dec -> do yield text
                                                       loop dec p'
                    DecodeResultFailure text bs -> do yield text 
                                                      return (do yield bs 
                                                                 p')
{-# INLINABLE decodeStream#-}




decodeUtf8 :: Monad m => Producer ByteString m r -> Producer Text m (Producer ByteString m r)
decodeUtf8 = decodeStream streamUtf8
{-# INLINE decodeUtf8 #-}

decodeUtf8Pure :: Monad m => Producer ByteString m r -> Producer Text m (Producer ByteString m r)
decodeUtf8Pure = decodeStream streamUtf8Pure
{-# INLINE decodeUtf8Pure #-}

decodeUtf16LE :: Monad m => Producer ByteString m r -> Producer Text m (Producer ByteString m r)
decodeUtf16LE = decodeStream streamUtf16LE
{-# INLINE decodeUtf16LE #-}

decodeUtf16BE :: Monad m => Producer ByteString m r -> Producer Text m (Producer ByteString m r)
decodeUtf16BE = decodeStream streamUtf16BE
{-# INLINE decodeUtf16BE #-}

decodeUtf32LE :: Monad m => Producer ByteString m r -> Producer Text m (Producer ByteString m r)
decodeUtf32LE = decodeStream streamUtf32LE
{-# INLINE decodeUtf32LE #-}

decodeUtf32BE :: Monad m => Producer ByteString m r -> Producer Text m (Producer ByteString m r)
decodeUtf32BE = decodeStream streamUtf32BE
{-# INLINE decodeUtf32BE #-}

mkCodec :: (forall r m . Monad m => 
           Producer ByteString m r -> Producer Text m (Producer ByteString m r ))
        -> (Text -> ByteString)
        -> Codec
mkCodec dec enc = \k p0 -> fmap (\p -> join (for p (yield . enc)))  (k (dec p0))


{- $codecs
    
    The particular \'Codec\' lenses are named in accordance with the expected encoding, 'utf8', 'utf16LE' etc.

>   view utf8 :: Producer ByteString m r -> Producer Text m (Producer ByteString m r)
>   Bytes.stdin ^. utf8 ::  Producer Text IO (Producer ByteString IO r)

    @zoom@ converts a Text parser into a ByteString parser:

>   zoom utf8 drawChar :: Monad m => StateT (Producer ByteString m r) m (Maybe Char)
> 
>   withNextByte :: Parser ByteString m (Maybe Char, Maybe Word8))) 
>   withNextByte = do char_ <- zoom utf8 Text.drawChar
>                     byte_ <- Bytes.peekByte
>                     return (char_, byte_)

     @withNextByte@ will return the first valid Char in a ByteString, 
     and the first byte of the next character, if they exists. Because 
     we \'draw\' one and \'peek\' at the other, the parser as a whole only 
     advances one Char's length along the bytestring.

    -}

utf8 :: Codec
utf8 = mkCodec decodeUtf8 TE.encodeUtf8

utf8Pure :: Codec
utf8Pure = mkCodec decodeUtf8Pure TE.encodeUtf8

utf16LE :: Codec
utf16LE = mkCodec decodeUtf16LE TE.encodeUtf16LE

utf16BE :: Codec
utf16BE = mkCodec decodeUtf16BE TE.encodeUtf16BE

utf32LE :: Codec
utf32LE = mkCodec decodeUtf32LE TE.encodeUtf32LE

utf32BE :: Codec
utf32BE = mkCodec decodeUtf32BE TE.encodeUtf32BE


{- $ascii
   ascii and latin encodings only use a small number of the characters 'Text'
     recognizes; thus we cannot use the pipes @Lens@ style to work with them. 
     Rather we simply define functions each way. 
-}


--  'encodeAscii' reduces as much of your stream of 'Text' actually is ascii to a byte stream,
--   returning the rest of the 'Text' at the first non-ascii 'Char'

encodeAscii :: Monad m => Producer Text m r -> Producer ByteString m (Producer Text m r)
encodeAscii = go where
  go p = do e <- lift (next p)
            case e of 
              Left r -> return (return r)
              Right (chunk, p') -> 
                 if T.null chunk 
                   then go p'
                   else let (safe, unsafe)  = T.span (\c -> ord c <= 0x7F) chunk
                        in do yield (B8.pack (T.unpack safe))
                              if T.null unsafe
                                then go p'
                                else return $ do yield unsafe 
                                                 p'
                                                 
{- | Reduce as much of your stream of 'Text' actually is iso8859 or latin1 to a byte stream,
     returning the rest of the 'Text' upon hitting any non-latin 'Char'
   -}
encodeIso8859_1 :: Monad m => Producer Text m r -> Producer ByteString m (Producer Text m r)
encodeIso8859_1 = go where
  go p = do e <- lift (next p)
            case e of 
              Left r -> return (return r)
              Right (txt, p') -> 
                 if T.null txt 
                   then go p'
                   else let (safe, unsafe)  = T.span (\c -> ord c <= 0xFF) txt
                        in do yield (B8.pack (T.unpack safe))
                              if T.null unsafe
                                then go p'
                                else return $ do yield unsafe 
                                                 p'

{- | Reduce a byte stream to a corresponding stream of ascii chars, returning the
     unused 'ByteString' upon hitting an un-ascii byte.
   -}
decodeAscii :: Monad m => Producer ByteString m r -> Producer Text m (Producer ByteString m r)
decodeAscii = go where
  go p = do e <- lift (next p)
            case e of 
              Left r -> return (return r)
              Right (chunk, p') -> 
                 if B.null chunk 
                   then go p'
                   else let (safe, unsafe) = B.span (<= 0x7F) chunk
                        in do yield (T.pack (B8.unpack safe))
                              if B.null unsafe
                                then go p'
                                else return (do yield unsafe 
                                                p')

{- | Reduce a byte stream to a corresponding stream of ascii chars, returning the
     unused 'ByteString' upon hitting the rare un-latinizable byte.
     -}
decodeIso8859_1 :: Monad m => Producer ByteString m r -> Producer Text m (Producer ByteString m r)
decodeIso8859_1 = go where
  go p = do e <- lift (next p)
            case e of 
              Left r -> return (return r)
              Right (chunk, p') -> 
                 if B.null chunk 
                    then go p'
                    else do let (safe, unsafe) = B.span (<= 0xFF) chunk
                            yield (T.pack (B8.unpack safe))
                            if B.null unsafe 
                               then go p'
                               else return (do yield unsafe 
                                               p')



