{-# LANGUAGE RankNTypes #-}

-- | This module uses the stream decoding functions from
--  <http://hackage.haskell.org/package/streaming-commons streaming-commons>
--  package to define decoding functions and lenses.  The exported names
--  conflict with names in @Data.Text.Encoding@ but not with the @Prelude@
module Pipes.Text.Encoding
  ( -- * Decoding ByteStrings and Encoding Texts

    -- ** Simple usage
    -- $usage

    -- ** Lens usage
    -- $lenses

    -- * Basic lens operations
    Codec,
    decode,
    eof,

    -- * Decoding lenses
    utf8,
    utf8Pure,
    utf16LE,
    utf16BE,
    utf32LE,
    utf32BE,

    -- * Non-lens decoding functions
    -- $decoders
    decodeUtf8,
    decodeUtf8Pure,
    decodeUtf16LE,
    decodeUtf16BE,
    decodeUtf32LE,
    decodeUtf32BE,

    -- * Re-encoding functions
    -- $encoders
    encodeUtf8,
    encodeUtf16LE,
    encodeUtf16BE,
    encodeUtf32LE,
    encodeUtf32BE,

    -- * Functions for latin and ascii text
    -- $ascii
    encodeAscii,
    decodeAscii,
    encodeIso8859_1,
    decodeIso8859_1,
  )
where

import Control.Monad (join)
import Data.ByteString as B
import Data.ByteString.Char8 as B8
import Data.Char (ord)
import Data.Functor.Constant (Constant (..))
import Data.Streaming.Text (DecodeResult (..))
import qualified Data.Streaming.Text as Stream
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Pipes

-- $usage
--    Encoding is of course simple. Given
--
-- >   text :: Producer Text IO ()
--
--    we can encode it with @Data.Text.Encoding.encodeUtf8@
--
-- >   TE.encodeUtf8 :: Text -> ByteString
--
--    and ordinary pipe operations:
--
-- >   text >-> P.map TE.encodeUtf8 :: Producer.ByteString IO ()
--
--    or, equivalently
--
-- >   for text (yield . TE.encodeUtf8)
--
--    But, using this module, we might use
--
-- >   encodeUtf8 :: Text -> Producer ByteString m ()
--
--    to write
--
-- >   for text encodeUtf8 :: Producer.ByteString IO ()
--
--    All of the above come to the same.
--
--
--    Given
--
-- >   bytes :: Producer ByteString IO ()
--
--    we can apply a decoding function from this module:
--
-- >   decodeUtf8 bytes :: Producer Text IO (Producer ByteString IO ())
--
--    The Text producer ends wherever decoding first fails. The un-decoded
--    material is returned. If we are confident it is of no interest, we can
--    write:
--
-- >   void $ decodeUtf8 bytes :: Producer Text IO ()
--
--    Thus we can re-encode
--    as uft8 as much of our byte stream as is decodeUtf16BE decodable, with, e.g.
--
-- >   for (decodeUtf16BE bytes) encodeUtf8 :: Producer ByteString IO (Producer ByteString IO ())
--
--    The bytestring producer that is returned begins with where utf16BE decoding
--    failed; if it didn't fail the producer is empty.

-- $lenses
--    We get a bit more flexibility, particularly in the use of pipes-style "parsers",
--    if we use a lens like @utf8@ or @utf16BE@
--    that focusses on the text in an appropriately encoded byte stream.
--
-- >   type Lens' a b = forall f . Functor f => (b -> f b) -> (a -> f a)
--
--    is just an alias for a Prelude type.  We abbreviate this further, for our use case, as
--
-- >   type Codec
-- >     =  forall m r .  Monad m => Lens' (Producer ByteString m r) (Producer Text m (Producer ByteString m r))
--
--    and call the decoding lenses @utf8@, @utf16BE@ \"codecs\", since they can
--    re-encode what they have decoded.  Thus you use any particular codec with
--    the @view@ / @(^.)@ , @zoom@ and @over@ functions from the standard lens libraries;
--    <http://hackage.haskell.org/package/lens lens>,
--    <http://hackage.haskell.org/package/lens-family lens-family>,
--    <http://hackage.haskell.org/package/lens-simple lens-simple>, or one of the
--    and <http://hackage.haskell.org/package/microlens microlens> packages will all work
--    the same, since we already have access to the types they require.
--
--    Each decoding lens looks into a byte stream that is supposed to contain text.
--    The particular lenses are named in accordance with the expected
--    encoding, 'utf8', 'utf16LE' etc. To turn a such a lens or @Codec@
--    into an ordinary function, use @view@ / @(^.)@ -- here also called 'decode':
--
-- >   view utf8 :: Producer ByteString m r -> Producer Text m (Producer ByteString m r)
-- >   decode utf8 Byte.stdin :: Producer Text IO (Producer ByteString IO r)
-- >   Bytes.stdin ^. utf8 ::  Producer Text IO (Producer ByteString IO r)
--
--    Of course, we could always do this with the specialized decoding functions, e.g.
--
-- >   decodeUtf8 ::  Producer ByteString m r -> Producer Text m (Producer ByteString m r)
-- >   decodeUtf8 Byte.stdin :: Producer Text IO (Producer ByteString IO r)
--
--    As with these functions, the stream of text that a @Codec@ \'sees\'
--    in the stream of bytes begins at its head.
--    At any point of decoding failure, the stream of text ends and reverts to (returns)
--    the original byte stream. Thus if the first bytes are already
--    un-decodable, the whole ByteString producer will be returned, i.e.
--
-- >   view utf8 bad_bytestream
--
--    will just come to the same as
--
-- >   return bad_bytestream
--
--    Where there is no decoding failure, the return value of the text stream will be
--    an empty byte stream followed by its own return value.  In all cases you must
--    deal with the fact that it is a /ByteString producer/ that is returned, even if
--    it can be thrown away with @Control.Monad.void@
--
-- >   void (Bytes.stdin ^. utf8) :: Producer Text IO ()
--
--    The @eof@ lens permits you to pattern match: if there is a Right value,
--    it is the leftover bytestring producer, if there is a Right value, it
--    is the return value of the original bytestring producer:
--
-- >   Bytes.stdin ^. utf8 . eof :: Producer Text IO (Either (Producer ByteString IO IO) ())
--
--    Thus for the stream of un-decodable bytes mentioned above,
--
-- >   view (utf8 . eof) bad_bytestream
--
--    will be the same as
--
-- >   return (Left bad_bytestream)
--
--    @zoom utf8@ converts a Text parser into a ByteString parser:
--
-- >   zoom utf8 drawChar :: Monad m => StateT (Producer ByteString m r) m (Maybe Char)
--
--    or, using the type synonymn from @Pipes.Parse@:
--
-- >   zoom utf8 drawChar :: Monad m => Parser ByteString m (Maybe Char)
--
--    Thus we can define a ByteString parser (in the pipes-parse sense) like this:
--
-- >   charPlusByte :: Parser ByteString m (Maybe Char, Maybe Word8)))
-- >   charPlusByte = do char_ <- zoom utf8 Text.drawChar
-- >                     byte_ <- Bytes.peekByte
-- >                     return (char_, byte_)
--
--     Though @charPlusByte@ is partly defined with a Text parser 'drawChar';
--     but it is a ByteString parser; it will return the first valid utf8-encoded
--     Char in a ByteString, /whatever its byte-length/,
--     and the first byte following, if both exist. Because
--     we \'draw\' one and \'peek\' at the other, the parser as a whole only
--     advances one Char's length along the bytestring, whatever that length may be.
--     See the slightly more complex example \'decode.hs\' in the
--     <http://www.haskellforall.com/2014/02/pipes-parse-30-lens-based-parsing.html#batteries-included haskellforall blog>
--     discussion of this type of byte stream parsing.

type Lens' a b = forall f. Functor f => (b -> f b) -> (a -> f a)

type Codec =
  forall m r.
  Monad m =>
  Lens'
    (Producer ByteString m r)
    (Producer Text m (Producer ByteString m r))

-- | @decode@ is just the ordinary @view@ or @(^.)@ of the lens libraries;
--   exported here under a name appropriate to the material.
--   Thus given a bytestring producer called @bytes@ we have
--
-- >    decode utf8 bytes :: Producer Text IO (Producer ByteString IO ())
--
--    All of these are thus the same:
--
-- >    decode utf8 bytes
-- >    view utf8 bytes
-- >    bytes ^. utf8
-- >    decodeUtf8 bytes
decode :: ((b -> Constant b b) -> (a -> Constant b a)) -> a -> b
decode codec a = getConstant (codec Constant a)

-- | @eof@ tells you explicitly when decoding stops due to bad bytes or
--    instead reaches end-of-file happily. (Without it one just makes an explicit
--    test for emptiness of the resulting bytestring production using next) Thus
--
-- >    decode (utf8 . eof) bytes :: Producer T.Text IO (Either (Producer B.ByteString IO ()) ())
--
--    If we hit undecodable bytes, the remaining bytestring producer will be
--    returned as a Left value; in the happy case, a Right value is returned
--    with the anticipated return value for the original bytestring producer.
--
--    Given a bytestring producer called @bytes@ all of these will be the same:
--
-- >    decode (utf8 . eof) bytes
-- >    view (utf8 . eof) bytes
-- >    bytes^.utf8.eof
eof ::
  (Monad m, Monad (t m), MonadTrans t) =>
  Lens'
    (t m (Producer ByteString m r))
    (t m (Either (Producer ByteString m r) r))
eof k p0 = fmap fromEither (k (toEither p0))
  where
    fromEither = fmap (either id return)

    toEither pp = do
      p <- pp
      check p

    check p = do
      e <- lift (next p)
      case e of
        Left r -> return (Right r)
        Right (bs, pb) ->
          if B.null bs
            then check pb
            else
              return
                ( Left
                    ( do
                        yield bs
                        pb
                    )
                )

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

decodeStream ::
  Monad m =>
  (B.ByteString -> DecodeResult) ->
  Producer ByteString m r ->
  Producer Text m (Producer ByteString m r)
decodeStream = loop
  where
    loop dec0 p =
      do
        x <- lift (next p)
        case x of
          Left r -> return (return r)
          Right (chunk, p') -> case dec0 chunk of
            DecodeResultSuccess text dec -> do
              yield text
              loop dec p'
            DecodeResultFailure text bs -> do
              yield text
              return
                ( do
                    yield bs
                    p'
                )
{-# INLINEABLE decodeStream #-}

-- $decoders
--   These are functions with the simple type:
--
-- >   decodeUtf8 :: Monad m => Producer ByteString m r -> Producer Text m (Producer ByteString m r)
--
--   Thus in general
--
-- >     decodeUtf8 = view utf8
-- >     decodeUtf16LE = view utf16LE
--
--   and so forth, but these forms
--   may be more convenient (and give better type errors!) where lenses are
--   not desired.

decodeUtf8 :: Monad m => Producer ByteString m r -> Producer Text m (Producer ByteString m r)
decodeUtf8 = decodeStream Stream.decodeUtf8
{-# INLINE decodeUtf8 #-}

decodeUtf8Pure :: Monad m => Producer ByteString m r -> Producer Text m (Producer ByteString m r)
decodeUtf8Pure = decodeStream Stream.decodeUtf8Pure
{-# INLINE decodeUtf8Pure #-}

decodeUtf16LE :: Monad m => Producer ByteString m r -> Producer Text m (Producer ByteString m r)
decodeUtf16LE = decodeStream Stream.decodeUtf16LE
{-# INLINE decodeUtf16LE #-}

decodeUtf16BE :: Monad m => Producer ByteString m r -> Producer Text m (Producer ByteString m r)
decodeUtf16BE = decodeStream Stream.decodeUtf16BE
{-# INLINE decodeUtf16BE #-}

decodeUtf32LE :: Monad m => Producer ByteString m r -> Producer Text m (Producer ByteString m r)
decodeUtf32LE = decodeStream Stream.decodeUtf32LE
{-# INLINE decodeUtf32LE #-}

decodeUtf32BE :: Monad m => Producer ByteString m r -> Producer Text m (Producer ByteString m r)
decodeUtf32BE = decodeStream Stream.decodeUtf32BE
{-# INLINE decodeUtf32BE #-}

-- $encoders
--   These are simply defined
--
-- >      encodeUtf8 = yield . TE.encodeUtf8
--
--   They are intended for use with 'for'
--
-- >      for Text.stdin encodeUtf8 :: Producer ByteString IO ()
--
--   which would have the effect of
--
-- >      Text.stdin >-> Pipes.Prelude.map (TE.encodeUtf8)
--
--   using the encoding functions from Data.Text.Encoding

encodeUtf8 :: Monad m => Text -> Producer' ByteString m ()
encodeUtf8 str = yield $ TE.encodeUtf8 str

encodeUtf16LE :: Monad m => Text -> Producer' ByteString m ()
encodeUtf16LE str = yield $ TE.encodeUtf16LE str

encodeUtf16BE :: Monad m => Text -> Producer' ByteString m ()
encodeUtf16BE str = yield $ TE.encodeUtf16BE str

encodeUtf32LE :: Monad m => Text -> Producer' ByteString m ()
encodeUtf32LE str = yield $ TE.encodeUtf32LE str

encodeUtf32BE :: Monad m => Text -> Producer' ByteString m ()
encodeUtf32BE str = yield $ TE.encodeUtf32BE str

mkCodec ::
  ( forall r m.
    Monad m =>
    Producer ByteString m r ->
    Producer Text m (Producer ByteString m r)
  ) ->
  (Text -> ByteString) ->
  Codec
mkCodec dec enc k p0 = fmap (\p -> join (for p (yield . enc))) (k (dec p0))

-- $ascii
--   ascii and latin encodings only use a small number of the characters 'Text'
--     recognizes; thus we cannot use the pipes @Lens@ style to work with them.
--     Rather we simply define functions each way.

-- | 'encodeAscii' reduces as much of your stream of 'Text' actually is ascii to a byte stream,
--   returning the rest of the 'Text' at the first non-ascii 'Char'
encodeAscii :: Monad m => Producer Text m r -> Producer ByteString m (Producer Text m r)
encodeAscii = go
  where
    go p = do
      e <- lift (next p)
      case e of
        Left r -> return (return r)
        Right (chunk, p') ->
          if T.null chunk
            then go p'
            else
              let (safe, unsafe) = T.span (\c -> ord c <= 0x7F) chunk
               in do
                    yield (B8.pack (T.unpack safe))
                    if T.null unsafe
                      then go p'
                      else return $ do
                        yield unsafe
                        p'

-- | Reduce as much of your stream of 'Text' actually is iso8859 or latin1 to a byte stream,
--     returning the rest of the 'Text' upon hitting any non-latin 'Char'
encodeIso8859_1 :: Monad m => Producer Text m r -> Producer ByteString m (Producer Text m r)
encodeIso8859_1 = go
  where
    go p = do
      e <- lift (next p)
      case e of
        Left r -> return (return r)
        Right (txt, p') ->
          if T.null txt
            then go p'
            else
              let (safe, unsafe) = T.span (\c -> ord c <= 0xFF) txt
               in do
                    yield (B8.pack (T.unpack safe))
                    if T.null unsafe
                      then go p'
                      else return $ do
                        yield unsafe
                        p'

-- | Reduce a byte stream to a corresponding stream of ascii chars, returning the
--     unused 'ByteString' upon hitting an un-ascii byte.
decodeAscii :: Monad m => Producer ByteString m r -> Producer Text m (Producer ByteString m r)
decodeAscii = go
  where
    go p = do
      e <- lift (next p)
      case e of
        Left r -> return (return r)
        Right (chunk, p') ->
          if B.null chunk
            then go p'
            else
              let (safe, unsafe) = B.span (<= 0x7F) chunk
               in do
                    yield (T.pack (B8.unpack safe))
                    if B.null unsafe
                      then go p'
                      else
                        return
                          ( do
                              yield unsafe
                              p'
                          )

-- | Reduce a byte stream to a corresponding stream of ascii chars, returning the
--     unused 'ByteString' upon hitting the rare un-latinizable byte.
decodeIso8859_1 :: Monad m => Producer ByteString m r -> Producer Text m (Producer ByteString m r)
decodeIso8859_1 = go
  where
    go p = do
      e <- lift (next p)
      case e of
        Left r -> return (return r)
        Right (chunk, p') ->
          if B.null chunk
            then go p'
            else do
              let (safe, unsafe) = B.span (<= 0xFF) chunk
              yield (T.pack (B8.unpack safe))
              if B.null unsafe
                then go p'
                else
                  return
                    ( do
                        yield unsafe
                        p'
                    )
