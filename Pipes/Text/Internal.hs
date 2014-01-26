{-# LANGUAGE BangPatterns, CPP, ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MagicHash, UnliftedFFITypes #-}
{-# LANGUAGE DeriveDataTypeable, RankNTypes #-}

-- This module lifts assorted materials from Brian O'Sullivan's text package 
-- especially Data.Text.Encoding in order to define a pipes-appropriate
-- streamDecodeUtf8
module Pipes.Text.Internal 
    ( Decoding(..)
    , streamDecodeUtf8
    , decodeSomeUtf8
    , Codec(..)
    , TextException(..)
    , utf8
    ) where
import Control.Monad.ST.Unsafe (unsafeIOToST, unsafeSTToIO)
import Control.Monad.ST (ST, runST)
import Data.Bits ((.&.))
import Data.ByteString as B 
import Data.ByteString (ByteString)
import Data.ByteString.Internal as B 
import Data.ByteString.Char8 as B8
import Data.Text (Text)
import qualified Data.Text as T 
import qualified Data.Text.Encoding as TE 
import Data.Text.Encoding.Error ()
import Data.Text.Internal (Text, textP)
import Foreign.C.Types (CSize)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, minusPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable, peek, poke)
import GHC.Base  (Char(..), Int(..), MutableByteArray#, ord#, iShiftRA#)
import GHC.Word (Word8, Word32)
import qualified Data.Text.Array as A
import Data.Word (Word8, Word16)
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Exception as Exc
import Data.Bits ((.&.), (.|.), shiftL)
import Data.Typeable
import Control.Arrow (first)
import Data.Maybe (catMaybes)
#include "pipes_text_cbits.h"


-- | A specific character encoding.
--
-- Since 0.3.0
data Codec = Codec
  { codecName :: Text
  , codecEncode :: Text -> (ByteString, Maybe (TextException, Text))
  , codecDecode :: ByteString -> Decoding -- (Text, Either (TextException, ByteString) ByteString)
  }

instance Show Codec where
    showsPrec d c = showParen (d > 10) $ showString "Codec " . shows (codecName c)

-- Since 0.3.0
data TextException = DecodeException Codec Word8
                   | EncodeException Codec Char
                   | LengthExceeded Int
                   | TextException Exc.SomeException
    deriving (Show, Typeable)
instance Exc.Exception TextException

toDecoding :: (ByteString -> (Text, Either (TextException, ByteString) ByteString))
           -> (ByteString -> Decoding)
toDecoding op = loop B.empty where
  loop extra bs0 = case op (B.append extra bs0) of
                    (txt, Right bs) -> Some txt bs (loop bs)
                    (txt, Left (_,bs)) -> Other txt bs


splitSlowly :: (ByteString -> Text)
            -> ByteString 
            -> (Text, Either (TextException, ByteString) ByteString)
splitSlowly dec bytes = valid where
    valid:_ = catMaybes $ Prelude.map decFirst $ splits (B.length bytes)
    splits 0 = [(B.empty, bytes)]
    splits n = B.splitAt n bytes : splits (n - 1)
    decFirst (a, b) = case tryEvaluate (dec a) of
        Left _ -> Nothing
        Right text -> let trouble = case tryEvaluate (dec b) of
                            Left exc -> Left (TextException exc, b)
                            Right _  -> Right B.empty 
                      in Just (text, trouble)
                                      -- this case shouldn't occur, 
                                      -- since splitSlowly is only called
                                      -- when parsing failed somewhere

utf8 :: Codec
utf8 = Codec name enc (toDecoding dec) where
    name = T.pack "UTF-8"
    enc text = (TE.encodeUtf8 text, Nothing)
    dec bytes = case decodeSomeUtf8 bytes of 
      (t,b) -> (t, Right b)

--     -- Whether the given byte is a continuation byte.
--     isContinuation byte = byte .&. 0xC0 == 0x80
-- 
--     -- The number of continuation bytes needed by the given
--     -- non-continuation byte. Returns -1 for an illegal UTF-8
--     -- non-continuation byte and the whole split quickly must fail so
--     -- as the input is passed to TE.decodeUtf8, which will issue a
--     -- suitable error.
--     required x0
--         | x0 .&. 0x80 == 0x00 = 0
--         | x0 .&. 0xE0 == 0xC0 = 1
--         | x0 .&. 0xF0 == 0xE0 = 2
--         | x0 .&. 0xF8 == 0xF0 = 3
--         | otherwise           = -1
-- 
--     splitQuickly bytes
--         | B.null l || req == -1 = Nothing
--         | req == B.length r = Just (TE.decodeUtf8 bytes, B.empty)
--         | otherwise = Just (TE.decodeUtf8 l', r')
--       where
--         (l, r) = B.spanEnd isContinuation bytes
--         req = required (B.last l)
--         l' = B.init l
--         r' = B.cons (B.last l) r

-- |
-- Since 0.3.0
utf16_le :: Codec
utf16_le = Codec name enc (toDecoding dec) where
    name = T.pack "UTF-16-LE"
    enc text = (TE.encodeUtf16LE text, Nothing)
    dec bytes = case splitQuickly bytes of
        Just (text, extra) -> (text, Right extra)
        Nothing -> splitSlowly TE.decodeUtf16LE bytes

    splitQuickly bytes = maybeDecode (loop 0) where
        maxN = B.length bytes

        loop n |  n      == maxN = decodeAll
               | (n + 1) == maxN = decodeTo n
        loop n = let
            req = utf16Required
                (B.index bytes n)
                (B.index bytes (n + 1))
            decodeMore = loop $! n + req
            in if n + req > maxN
                then decodeTo n
                else decodeMore

        decodeTo n = first TE.decodeUtf16LE (B.splitAt n bytes)
        decodeAll = (TE.decodeUtf16LE bytes, B.empty)

-- |
-- Since 0.3.0
utf16_be :: Codec
utf16_be = Codec name enc (toDecoding dec) where
    name = T.pack "UTF-16-BE"
    enc text = (TE.encodeUtf16BE text, Nothing)
    dec bytes = case splitQuickly bytes of
        Just (text, extra) -> (text, Right extra)
        Nothing -> splitSlowly TE.decodeUtf16BE bytes

    splitQuickly bytes = maybeDecode (loop 0) where
        maxN = B.length bytes

        loop n |  n      == maxN = decodeAll
               | (n + 1) == maxN = decodeTo n
        loop n = let
            req = utf16Required
                (B.index bytes (n + 1))
                (B.index bytes n)
            decodeMore = loop $! n + req
            in if n + req > maxN
                then decodeTo n
                else decodeMore

        decodeTo n = first TE.decodeUtf16BE (B.splitAt n bytes)
        decodeAll = (TE.decodeUtf16BE bytes, B.empty)

utf16Required :: Word8 -> Word8 -> Int
utf16Required x0 x1 = if x >= 0xD800 && x <= 0xDBFF then 4 else 2 where
    x :: Word16
    x = (fromIntegral x1 `shiftL` 8) .|. fromIntegral x0

-- |
-- Since 0.3.0
utf32_le :: Codec
utf32_le = Codec name enc (toDecoding dec) where
    name = T.pack "UTF-32-LE"
    enc text = (TE.encodeUtf32LE text, Nothing)
    dec bs = case utf32SplitBytes TE.decodeUtf32LE bs of
        Just (text, extra) -> (text, Right extra)
        Nothing -> splitSlowly TE.decodeUtf32LE bs

-- |
-- Since 0.3.0
utf32_be :: Codec
utf32_be = Codec name enc (toDecoding dec) where
    name = T.pack "UTF-32-BE"
    enc text = (TE.encodeUtf32BE text, Nothing)
    dec bs = case utf32SplitBytes TE.decodeUtf32BE bs of
        Just (text, extra) -> (text, Right extra)
        Nothing -> splitSlowly TE.decodeUtf32BE bs

utf32SplitBytes :: (ByteString -> Text)
                -> ByteString
                -> Maybe (Text, ByteString)
utf32SplitBytes dec bytes = split where
    split = maybeDecode (dec toDecode, extra)
    len = B.length bytes
    lenExtra = mod len 4

    lenToDecode = len - lenExtra
    (toDecode, extra) = if lenExtra == 0
        then (bytes, B.empty)
        else B.splitAt lenToDecode bytes

-- |
-- Since 0.3.0
ascii :: Codec
ascii = Codec name enc (toDecoding dec) where
    name = T.pack "ASCII"
    enc text = (bytes, extra) where
        (safe, unsafe) = T.span (\c -> ord c <= 0x7F) text
        bytes = B8.pack (T.unpack safe)
        extra = if T.null unsafe
            then Nothing
            else Just (EncodeException ascii (T.head unsafe), unsafe)

    dec bytes = (text, extra) where
        (safe, unsafe) = B.span (<= 0x7F) bytes
        text = T.pack (B8.unpack safe)
        extra = if B.null unsafe
            then Right B.empty
            else Left (DecodeException ascii (B.head unsafe), unsafe)

-- |
-- Since 0.3.0
iso8859_1 :: Codec
iso8859_1 = Codec name enc (toDecoding dec) where
    name = T.pack "ISO-8859-1"
    enc text = (bytes, extra) where
        (safe, unsafe) = T.span (\c -> ord c <= 0xFF) text
        bytes = B8.pack (T.unpack safe)
        extra = if T.null unsafe
            then Nothing
            else Just (EncodeException iso8859_1 (T.head unsafe), unsafe)

    dec bytes = (T.pack (B8.unpack bytes), Right B.empty)

tryEvaluate :: a -> Either Exc.SomeException a
tryEvaluate = unsafePerformIO . Exc.try . Exc.evaluate

maybeDecode :: (a, b) -> Maybe (a, b)
maybeDecode (a, b) = case tryEvaluate a of
    Left _ -> Nothing
    Right _ -> Just (a, b)

-- | A stream oriented decoding result.
data Decoding = Some Text ByteString (ByteString -> Decoding)
              | Other Text ByteString
instance Show Decoding where
    showsPrec d (Some t bs _) = showParen (d > prec) $
                                showString "Some " . showsPrec prec' t .
                                showChar ' ' . showsPrec prec' bs .
                                showString " _"
      where prec = 10; prec' = prec + 1
    showsPrec d (Other t bs)  = showParen (d > prec) $
                                showString "Other " . showsPrec prec' t .
                                showChar ' ' . showsPrec prec' bs .
                                showString " _"
      where prec = 10; prec' = prec + 1

newtype CodePoint = CodePoint Word32 deriving (Eq, Show, Num, Storable)
newtype DecoderState = DecoderState Word32 deriving (Eq, Show, Num, Storable)

streamDecodeUtf8 :: ByteString -> Decoding
streamDecodeUtf8 = decodeChunkUtf8 B.empty 0 0 
  where
  decodeChunkUtf8 :: ByteString -> CodePoint -> DecoderState -> ByteString -> Decoding
  decodeChunkUtf8 old codepoint0 state0 bs@(PS fp off len) = 
                    runST $ do marray <- A.new (len+1) 
                               unsafeIOToST (decodeChunkToBuffer marray)
     where
     decodeChunkToBuffer :: A.MArray s -> IO Decoding
     decodeChunkToBuffer dest = withForeignPtr fp $ \ptr ->
       with (0::CSize) $ \destOffPtr ->
       with codepoint0 $ \codepointPtr ->
       with state0     $ \statePtr ->
       with nullPtr    $ \curPtrPtr ->
         do let end = ptr `plusPtr` (off + len)
                curPtr = ptr `plusPtr` off
            poke curPtrPtr curPtr
            c_decode_utf8_with_state (A.maBA dest) destOffPtr curPtrPtr end codepointPtr statePtr
            state <- peek statePtr
            lastPtr <- peek curPtrPtr
            codepoint <- peek codepointPtr
            n <- peek destOffPtr
            chunkText <- mkText dest n
            let left      = lastPtr `minusPtr` curPtr
                remaining = B.drop left bs
                accum = if T.null chunkText then B.append old remaining  else remaining 
            return $! case state of 
              UTF8_REJECT -> Other chunkText accum -- We encountered an encoding error
              _ ->           Some  chunkText accum (decodeChunkUtf8 accum codepoint state)
     {-# INLINE decodeChunkToBuffer #-}
  {-# INLINE decodeChunkUtf8 #-}
{-# INLINE streamDecodeUtf8 #-}

decodeSomeUtf8 :: ByteString -> (Text, ByteString)
decodeSomeUtf8 bs@(PS fp off len) = runST $ do 
  dest <- A.new (len+1) 
  unsafeIOToST $ 
     withForeignPtr fp $ \ptr ->
     with (0::CSize)        $ \destOffPtr ->
     with (0::CodePoint)    $ \codepointPtr ->
     with (0::DecoderState) $ \statePtr ->
     with nullPtr           $ \curPtrPtr ->
       do let end = ptr `plusPtr` (off + len)
              curPtr = ptr `plusPtr` off
          poke curPtrPtr curPtr
          c_decode_utf8_with_state (A.maBA dest) destOffPtr 
                                   curPtrPtr end codepointPtr statePtr
          state <- peek statePtr
          lastPtr <- peek curPtrPtr
          codepoint <- peek codepointPtr
          n <- peek destOffPtr
          chunkText <- unsafeSTToIO $ do arr <- A.unsafeFreeze dest
                                         return $! textP arr 0 (fromIntegral n)
          let left      = lastPtr `minusPtr` curPtr
              remaining = B.drop left bs
          return $! (chunkText, remaining)
{-# INLINE decodeSomeUtf8 #-}

mkText :: A.MArray s -> CSize -> IO Text
mkText dest n =  unsafeSTToIO $ do arr <- A.unsafeFreeze dest
                                   return $! textP arr 0 (fromIntegral n)
{-# INLINE mkText #-}

ord :: Char -> Int
ord (C# c#) = I# (ord# c#)
{-# INLINE ord #-}

unsafeWrite :: A.MArray s -> Int -> Char -> ST s Int
unsafeWrite marr i c
    | n < 0x10000 = do A.unsafeWrite marr i (fromIntegral n)
                       return 1
    | otherwise   = do A.unsafeWrite marr i lo
                       A.unsafeWrite marr (i+1) hi
                       return 2
    where n = ord c
          m = n - 0x10000
          lo = fromIntegral $ (m `shiftR` 10) + 0xD800
          hi = fromIntegral $ (m .&. 0x3FF) + 0xDC00
          shiftR (I# x#) (I# i#) = I# (x# `iShiftRA#` i#)
          {-# INLINE shiftR #-}
{-# INLINE unsafeWrite #-}

foreign import ccall unsafe "_hs_pipes_text_decode_utf8_state" c_decode_utf8_with_state
    :: MutableByteArray# s -> Ptr CSize
    -> Ptr (Ptr Word8) -> Ptr Word8
    -> Ptr CodePoint -> Ptr DecoderState -> IO (Ptr Word8)