{-# LANGUAGE BangPatterns, CPP, ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MagicHash, UnliftedFFITypes #-}
{-# LANGUAGE DeriveDataTypeable, RankNTypes #-}

-- This module lifts assorted materials from Brian O'Sullivan's text package 
-- especially Data.Text.Encoding in order to define a pipes-appropriate
-- streamDecodeUtf8
module Pipes.Text.Internal.Decoding 
    ( Decoding(..)
    , streamDecodeUtf8
    , decodeSomeUtf8
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