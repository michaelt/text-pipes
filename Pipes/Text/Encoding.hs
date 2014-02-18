{-# LANGUAGE RankNTypes, BangPatterns #-}

-- | This module uses the stream decoding functions from Michael Snoyman's new
--  <http://hackage.haskell.org/package/text-stream-decode text-stream-decode> 
--  package to define decoding functions and lenses.  

module Pipes.Text.Encoding
    ( 
    -- * The Lens or Codec type
    -- $lenses
    Codec
    -- * Viewing the Text in a ByteString
    -- $codecs
    , decode
    , utf8
    , utf8Pure
    , utf16LE
    , utf16BE
    , utf32LE
    , utf32BE
    -- * Non-lens decoding functions 
    -- $decoders
    , decodeUtf8
    , decodeUtf8Pure
    , decodeUtf16LE
    , decodeUtf16BE
    , decodeUtf32LE
    , decodeUtf32BE
    -- * Re-encoding functions
    -- $encoders
    , encodeUtf8
    , encodeUtf16LE
    , encodeUtf16BE
    , encodeUtf32LE
    , encodeUtf32BE
    -- * Functions for latin and ascii text
    -- $ascii
    , encodeAscii
    , decodeAscii
    , encodeIso8859_1
    , decodeIso8859_1
    ) 
    where

import Data.Functor.Constant (Constant(..))
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
    The 'Codec' type is a simple specializion of 
    the @Lens'@ type synonymn used by the standard lens libraries, 
    <http://hackage.haskell.org/package/lens lens> and 
    <http://hackage.haskell.org/package/lens-family lens-family>. That type, 
    
>   type Lens' a b = forall f . Functor f => (b -> f b) -> (a -> f a)

    is just an alias for an ordinary Prelude type.  Thus you use any codec with
    the @view@ / @(^.)@ and @zoom@ functions from those libraries.

    -}

type Codec
    =  forall m r
    .  Monad m
    => Lens' (Producer ByteString m r)
             (Producer Text m (Producer ByteString m r))
             
{- | 'decode' is just the ordinary @view@ or @(^.)@ of the lens libraries;
      exported here for convience

>    decode utf8 p = decodeUtf8 p = view utf8 p = p ^. utf

-}

decode :: ((b -> Constant b b) -> (a -> Constant b a)) -> a -> b
decode codec a = getConstant (codec Constant a)


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

{- $decoders
   These are functions with the simple type:
   
>   decodeUtf8 :: Monad m => Producer ByteString m r -> Producer Text m (Producer ByteString m r)

   Thus in general 

>     decodeUtf8 = view utf8
>     decodeUtf16LE = view utf16LE

   and so forth, but these forms
   may be more convenient (and give better type errors!) where lenses are
   not desired.
-}


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


{- $encoders
   These are simply defined 
   
>      encodeUtf8 = yield . TE.encodeUtf8
   
   They are intended for use with 'for'
   
>      for Text.stdin encodeUtf8 :: Producer ByteString IO ()

   which would have the effect of 
   
>      Text.stdin >-> Pipes.Prelude.map (TE.encodeUtf8)

   using the encoding functions from Data.Text.Encoding 
-}

encodeUtf8 :: Monad m => Text -> Producer ByteString m ()
encodeUtf8 = yield . TE.encodeUtf8
encodeUtf16LE :: Monad m => Text -> Producer ByteString m ()
encodeUtf16LE = yield . TE.encodeUtf16LE
encodeUtf16BE :: Monad m => Text -> Producer ByteString m ()
encodeUtf16BE = yield . TE.encodeUtf16BE
encodeUtf32LE :: Monad m => Text -> Producer ByteString m ()
encodeUtf32LE = yield . TE.encodeUtf32LE
encodeUtf32BE :: Monad m => Text -> Producer ByteString m ()
encodeUtf32BE = yield . TE.encodeUtf32BE

mkCodec :: (forall r m . Monad m => 
           Producer ByteString m r -> Producer Text m (Producer ByteString m r ))
        -> (Text -> ByteString)
        -> Codec
mkCodec dec enc = \k p0 -> fmap (\p -> join (for p (yield . enc)))  (k (dec p0))


{- $codecs
    
    Each codec/lens looks into a byte stream that is supposed to contain text.
    The particular \'Codec\' lenses are named in accordance with the expected 
    encoding, 'utf8', 'utf16LE' etc. @view@ / @(^.)@ -- here also called 'decode' -- 
    turns a Codec into a function:

>   view utf8 :: Producer ByteString m r -> Producer Text m (Producer ByteString m r)
>   decode utf8 Byte.stdin :: Producer Text IO (Producer ByteString IO r)
>   Bytes.stdin ^. utf8 ::  Producer Text IO (Producer ByteString IO r)

    Uses of a codec with @view@ or @(^.)@ or 'decode' can always be replaced by the specialized 
    decoding functions exported here, e.g. 

>   decodeUtf8 ::  Producer ByteString m r -> Producer Text m (Producer ByteString m r)
>   decodeUtf8 Byte.stdin :: Producer Text IO (Producer ByteString IO r)

    The stream of text a @Codec@ \'sees\' in the stream of bytes begins at its head. 
    At any point of decoding failure, the stream of text ends and reverts to (returns) 
    the original byte stream. Thus if the first bytes are already
    un-decodable, the whole ByteString producer will be returned, i.e.

>   view utf8 bytestream 

    will just come to the same as 

>   return bytestream

    Where there is no decoding failure, the return value of the text stream will be
    an empty byte stream followed by its own return value.  In all cases you must
    deal with the fact that it is a ByteString producer that is returned, even if
    it can be thrown away with @Control.Monad.void@

>   void (Bytes.stdin ^. utf8) :: Producer Text IO ()
    
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


-- | 'encodeAscii' reduces as much of your stream of 'Text' actually is ascii to a byte stream,
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



