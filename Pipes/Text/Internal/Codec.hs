
{-# LANGUAGE DeriveDataTypeable, RankNTypes, BangPatterns #-}
-- |
-- Copyright: 2014 Michael Thompson, 2011 Michael Snoyman, 2010-2011 John Millikin
-- License: MIT
--
-- Parts of this code were taken from enumerator and conduits, and adapted for pipes.

module Pipes.Text.Internal.Codec
    ( Decoding(..)
    , streamDecodeUtf8
    , decodeSomeUtf8
    , Codec(..)
    , TextException(..)
    , utf8
    , utf16_le
    , utf16_be
    , utf32_le
    , utf32_be
    ) where

import Data.Bits ((.&.))
import Data.Char (ord)
import Data.ByteString as B 
import Data.ByteString (ByteString)
import Data.ByteString.Internal as B 
import Data.ByteString.Char8 as B8
import Data.Text (Text)
import qualified Data.Text as T 
import qualified Data.Text.Encoding as TE 
import Data.Text.Encoding.Error ()
import GHC.Word (Word8, Word32)
import qualified Data.Text.Array as A
import Data.Word (Word8, Word16)
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Exception as Exc
import Data.Bits ((.&.), (.|.), shiftL)
import Data.Typeable
import Control.Arrow (first)
import Data.Maybe (catMaybes)
import Pipes.Text.Internal.Decoding
import Pipes
-- | A specific character encoding.
--
-- Since 0.3.0
data Codec = Codec
  { codecName :: Text
  , codecEncode :: Text -> (ByteString, Maybe (TextException, Text))
  , codecDecode :: ByteString -> Decoding -- (Text, Either (TextException, ByteString) ByteString)
  }

instance Show Codec where
    showsPrec d c = showParen (d > 10) $ 
                    showString "Codec " . shows (codecName c)

data TextException = DecodeException Codec Word8
                   | EncodeException Codec Char
                   | LengthExceeded Int
                   | TextException Exc.SomeException
    deriving (Show, Typeable)
instance Exc.Exception TextException


toDecoding :: (ByteString -> (Text, Either (TextException, ByteString) ByteString))
           -> (ByteString -> Decoding)
toDecoding op = loop B.empty where
  loop !extra bs0 = case op (B.append extra bs0) of
                      (txt, Right bs) -> Some txt bs (loop bs)
                      (txt, Left (_,bs)) -> Other txt bs
-- To do: toDecoding should be inlined in each of the 'Codec' definitions
-- or else Codec changed to the conduit/enumerator definition.  We have
-- altered it to use 'streamDecodeUtf8'

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
                      in Just (text, trouble) -- this case shouldn't occur, 
                                      -- since splitSlowly is only called
                                      -- when parsing failed somewhere

utf8 :: Codec
utf8 = Codec name enc (toDecoding dec) where
    name = T.pack "UTF-8"
    enc text = (TE.encodeUtf8 text, Nothing)
    dec bytes = case decodeSomeUtf8 bytes of (t,b) -> (t, Right b)

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


utf32_le :: Codec
utf32_le = Codec name enc (toDecoding dec) where
    name = T.pack "UTF-32-LE"
    enc text = (TE.encodeUtf32LE text, Nothing)
    dec bs = case utf32SplitBytes TE.decodeUtf32LE bs of
        Just (text, extra) -> (text, Right extra)
        Nothing -> splitSlowly TE.decodeUtf32LE bs


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


tryEvaluate :: a -> Either Exc.SomeException a
tryEvaluate = unsafePerformIO . Exc.try . Exc.evaluate

maybeDecode :: (a, b) -> Maybe (a, b)
maybeDecode (a, b) = case tryEvaluate a of
    Left _ -> Nothing
    Right _ -> Just (a, b)
