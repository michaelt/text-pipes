{-# LANGUAGE RankNTypes, TypeFamilies, BangPatterns, Trustworthy #-}

{-| This module provides @pipes@ utilities for \"text streams\", which are
    streams of 'Text' chunks. The individual chunks are uniformly @strict@, but 
    a 'Producer' can be converted to and from lazy 'Text's, though this is generally 
    unwise.  Where pipes IO replaces lazy IO, 'Producer Text m r' replaces lazy 'Text'.
    An 'IO.Handle' can be associated with a 'Producer' or 'Consumer' according as it is read or written to.

    To stream to or from 'IO.Handle's, one can use 'fromHandle' or 'toHandle'.  For
    example, the following program copies a document from one file to another:

> import Pipes
> import qualified Data.Text.Pipes as Text
> import System.IO
>
> main =
>     withFile "inFile.txt"  ReadMode  $ \hIn  ->
>     withFile "outFile.txt" WriteMode $ \hOut ->
>     runEffect $ Text.fromHandle hIn >-> Text.toHandle hOut

To stream from files, the following is perhaps more Prelude-like (note that it uses Pipes.Safe):

> import Pipes
> import qualified Data.Text.Pipes as Text
> import Pipes.Safe
>
> main = runSafeT $ runEffect $ Text.readFile "inFile.txt" >-> Text.writeFile "outFile.txt"

    You can stream to and from 'stdin' and 'stdout' using the predefined 'stdin'
    and 'stdout' pipes, as with the following \"echo\" program:

> main = runEffect $ Text.stdin >-> Text.stdout

    You can also translate pure lazy 'TL.Text's to and from pipes:

> main = runEffect $ Text.fromLazy (TL.pack "Hello, world!\n") >-> Text.stdout

    In addition, this module provides many functions equivalent to lazy
    'Text' functions so that you can transform or fold text streams.  For
    example, to stream only the first three lines of 'stdin' to 'stdout' you
    might write:

> import Pipes
> import qualified Pipes.Text as Text
> import qualified Pipes.Parse as Parse
>
> main = runEffect $ takeLines 3 Text.stdin >-> Text.stdout
>   where
>     takeLines n = Text.unlines . Parse.takeFree n . Text.lines

    The above program will never bring more than one chunk of text (~ 32 KB) into
    memory, no matter how long the lines are.

    Note that functions in this library are designed to operate on streams that
    are insensitive to text boundaries.  This means that they may freely split
    text into smaller texts, /discard empty texts/.  However, apart from the 
    special case of 'concatMap', they will /never concatenate texts/ in order 
    to provide strict upper bounds on memory usage -- with the single exception of 'concatMap'.  
-}

module Pipes.Text  (
    -- * Producers
      fromLazy
    , stdin
    , fromHandle
    , readFile

    -- * Consumers
    , stdout
    , toHandle
    , writeFile

    -- * Pipes
    , map
    , concatMap
    , take
    , drop
    , takeWhile
    , dropWhile
    , filter
    , scan
    , encodeUtf8
    , pack
    , unpack
    , toCaseFold
    , toLower
    , toUpper
    , stripStart

    -- * Folds
    , toLazy
    , toLazyM
    , foldChars
    , head
    , last
    , null
    , length
    , any
    , all
    , maximum
    , minimum
    , find
    , index
    , count

    -- * Primitive Character Parsers
    -- $parse
    , nextChar
    , drawChar
    , unDrawChar
    , peekChar
    , isEndOfChars

    -- * Parsing Lenses 
    , splitAt
    , span
    , break
    , groupBy
    , group
    , word
    , line
    
    -- * Decoding Lenses 
    , decodeUtf8
    , codec
    
    -- * Codecs
    , utf8
    , utf16_le
    , utf16_be
    , utf32_le
    , utf32_be
    
    -- * Other Decoding/Encoding Functions
    , decodeIso8859_1
    , decodeAscii
    , encodeIso8859_1
    , encodeAscii

    -- * FreeT Splitters
    , chunksOf
    , splitsWith
    , splits
--  , groupsBy
--  , groups
    , lines
    , words

    -- * Transformations
    , intersperse
    , packChars
    
    -- * Joiners
    , intercalate
    , unlines
    , unwords

   -- * Re-exports
    -- $reexports
    , Decoding(..)
    , streamDecodeUtf8
    , decodeSomeUtf8
    , Codec(..)
    , TextException(..)
    , module Data.ByteString
    , module Data.Text
    , module Data.Profunctor
    , module Data.Word
    , module Pipes.Parse
    , module Pipes.Group
    ) where

import Control.Exception (throwIO, try)
import Control.Applicative ((<*)) 
import Control.Monad (liftM, unless, join)
import Control.Monad.Trans.State.Strict (StateT(..), modify)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Text.Lazy.Internal (foldrChunks, defaultChunkSize)
import Data.ByteString.Unsafe (unsafeTake, unsafeDrop)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Char (ord, isSpace)
import Data.Functor.Constant (Constant(Constant, getConstant))
import Data.Functor.Identity (Identity)
import Data.Profunctor (Profunctor)
import qualified Data.Profunctor
import qualified Data.List as List
import Foreign.C.Error (Errno(Errno), ePIPE)
import qualified GHC.IO.Exception as G
import Pipes
import qualified Pipes.ByteString as PB
import qualified Pipes.Text.Internal as PI
import Pipes.Text.Internal 
import Pipes.Core (respond, Server')
import Pipes.Group (concats, intercalates, FreeT(..), FreeF(..))
import qualified Pipes.Group as PG
import qualified Pipes.Parse as PP
import Pipes.Parse (Parser)
import qualified Pipes.Safe.Prelude as Safe
import qualified Pipes.Safe as Safe
import Pipes.Safe (MonadSafe(..), Base(..))
import qualified Pipes.Prelude as P
import qualified System.IO as IO
import Data.Char (isSpace)
import Data.Word (Word8)

import Prelude hiding (
    all,
    any,
    break,
    concat,
    concatMap,
    drop,
    dropWhile,
    elem,
    filter,
    head,
    last,
    lines,
    length,
    map,
    maximum,
    minimum,
    notElem,
    null,
    readFile,
    span,
    splitAt,
    take,
    takeWhile,
    unlines,
    unwords,
    words,
    writeFile )

-- | Convert a lazy 'TL.Text' into a 'Producer' of strict 'Text's
fromLazy :: (Monad m) => TL.Text -> Producer' Text m ()
fromLazy  = foldrChunks (\e a -> yield e >> a) (return ()) 
{-# INLINE fromLazy #-}

-- | Stream text from 'stdin'
stdin :: MonadIO m => Producer Text m ()
stdin = fromHandle IO.stdin
{-# INLINE stdin #-}

{-| Convert a 'IO.Handle' into a text stream using a text size 
    determined by the good sense of the text library; note that this
    is distinctly slower than @decideUtf8 (Pipes.ByteString.fromHandle h)@
    but uses the system encoding and has other `Data.Text.IO` features
-}

fromHandle :: MonadIO m => IO.Handle -> Producer Text m ()
fromHandle h =  go where
      go = do txt <- liftIO (T.hGetChunk h)
              unless (T.null txt) ( do yield txt
                                       go )
{-# INLINABLE fromHandle#-}


{-| Stream text from a file in the simple fashion of @Data.Text.IO@ 

>>> runSafeT $ runEffect $ Text.readFile "hello.hs" >-> Text.map toUpper >-> hoist lift Text.stdout
MAIN = PUTSTRLN "HELLO WORLD"
-}

readFile :: MonadSafe m => FilePath -> Producer Text m ()
readFile file = Safe.withFile file IO.ReadMode fromHandle
{-# INLINE readFile #-}


{-| Stream text to 'stdout'

    Unlike 'toHandle', 'stdout' gracefully terminates on a broken output pipe.

    Note: For best performance, it might be best just to use @(for source (liftIO . putStr))@ 
    instead of @(source >-> stdout)@ .
-}
stdout :: MonadIO m => Consumer' Text m ()
stdout = go
  where
    go = do
        txt <- await
        x  <- liftIO $ try (T.putStr txt)
        case x of
            Left (G.IOError { G.ioe_type  = G.ResourceVanished
                            , G.ioe_errno = Just ioe })
                 | Errno ioe == ePIPE
                     -> return ()
            Left  e  -> liftIO (throwIO e)
            Right () -> go
{-# INLINABLE stdout #-}


{-| Convert a text stream into a 'Handle'

    Note: again, for best performance, where possible use 
    @(for source (liftIO . hPutStr handle))@ instead of @(source >-> toHandle handle)@.
-}
toHandle :: MonadIO m => IO.Handle -> Consumer' Text m r
toHandle h = for cat (liftIO . T.hPutStr h)
{-# INLINABLE toHandle #-}

{-# RULES "p >-> toHandle h" forall p h .
        p >-> toHandle h = for p (\txt -> liftIO (T.hPutStr h txt))
  #-}


-- | Stream text into a file. Uses @pipes-safe@.
writeFile :: (MonadSafe m) => FilePath -> Consumer' Text m ()
writeFile file = Safe.withFile file IO.WriteMode toHandle
{-# INLINE writeFile #-}


type Lens' a b = forall f . Functor f => (b -> f b) -> (a -> f a)

type Iso' a b = forall f p . (Functor f, Profunctor p) => p b (f b) -> p a (f a)

(^.) :: a -> ((b -> Constant b b) -> (a -> Constant b a)) -> b
a ^. lens = getConstant (lens Constant a)


-- | Apply a transformation to each 'Char' in the stream
map :: (Monad m) => (Char -> Char) -> Pipe Text Text m r
map f = P.map (T.map f)
{-# INLINABLE map #-}

{-# RULES "p >-> map f" forall p f .
        p >-> map f = for p (\txt -> yield (T.map f txt))
  #-}

-- | Map a function over the characters of a text stream and concatenate the results
concatMap
    :: (Monad m) => (Char -> Text) -> Pipe Text Text m r
concatMap f = P.map (T.concatMap f)
{-# INLINABLE concatMap #-}

{-# RULES "p >-> concatMap f" forall p f .
        p >-> concatMap f = for p (\txt -> yield (T.concatMap f txt))
  #-}

-- | Transform a Pipe of 'Text' into a Pipe of 'ByteString's using UTF-8
-- encoding; @encodeUtf8 = Pipes.Prelude.map TE.encodeUtf8@ so more complex
-- encoding pipes can easily be constructed with the functions in @Data.Text.Encoding@
encodeUtf8 :: Monad m => Pipe Text ByteString m r
encodeUtf8 = P.map TE.encodeUtf8
{-# INLINEABLE encodeUtf8 #-}

{-# RULES "p >-> encodeUtf8" forall p .
        p >-> encodeUtf8 = for p (\txt -> yield (TE.encodeUtf8 txt))
  #-}

-- | Transform a Pipe of 'String's into one of 'Text' chunks
pack :: Monad m => Pipe String Text m r
pack = P.map T.pack
{-# INLINEABLE pack #-}

{-# RULES "p >-> pack" forall p .
        p >-> pack = for p (\txt -> yield (T.pack txt))
  #-}

-- | Transform a Pipes of 'Text' chunks into one of 'String's
unpack :: Monad m => Pipe Text String m r
unpack = for cat (\t -> yield (T.unpack t))
{-# INLINEABLE unpack #-}

{-# RULES "p >-> unpack" forall p .
        p >-> unpack = for p (\txt -> yield (T.unpack txt))
  #-}

-- | @toCaseFold@, @toLower@, @toUpper@ and @stripStart@ are standard 'Text' utilities, 
-- here acting as 'Text' pipes, rather as they would  on a lazy text
toCaseFold :: Monad m => Pipe Text Text m ()
toCaseFold = P.map T.toCaseFold
{-# INLINEABLE toCaseFold #-}

{-# RULES "p >-> toCaseFold" forall p .
        p >-> toCaseFold = for p (\txt -> yield (T.toCaseFold txt))
  #-}


-- | lowercase incoming 'Text'
toLower :: Monad m => Pipe Text Text m ()
toLower = P.map T.toLower
{-# INLINEABLE toLower #-}

{-# RULES "p >-> toLower" forall p .
        p >-> toLower = for p (\txt -> yield (T.toLower txt))
  #-}

-- | uppercase incoming 'Text'
toUpper :: Monad m => Pipe Text Text m ()
toUpper = P.map T.toUpper
{-# INLINEABLE toUpper #-}

{-# RULES "p >-> toUpper" forall p .
        p >-> toUpper = for p (\txt -> yield (T.toUpper txt))
  #-}

-- | Remove leading white space from an incoming succession of 'Text's 
stripStart :: Monad m => Pipe Text Text m r
stripStart = do
    chunk <- await
    let text = T.stripStart chunk
    if T.null text
      then stripStart
      else do yield text 
              cat
{-# INLINEABLE stripStart #-}

-- | @(take n)@ only allows @n@ individual characters to pass; 
--  contrast @Pipes.Prelude.take@ which would let @n@ chunks pass.
take :: (Monad m, Integral a) => a -> Pipe Text Text m ()
take n0 = go n0 where
    go n
        | n <= 0    = return ()
        | otherwise = do
            txt <- await
            let len = fromIntegral (T.length txt)
            if (len > n)
                then yield (T.take (fromIntegral n) txt)
                else do
                    yield txt
                    go (n - len)
{-# INLINABLE take #-}

-- | @(drop n)@ drops the first @n@ characters
drop :: (Monad m, Integral a) => a -> Pipe Text Text m r
drop n0 = go n0 where
    go n
        | n <= 0    = cat
        | otherwise = do
            txt <- await
            let len = fromIntegral (T.length txt)
            if (len >= n)
                then do
                    yield (T.drop (fromIntegral n) txt)
                    cat
                else go (n - len)
{-# INLINABLE drop #-}

-- | Take characters until they fail the predicate
takeWhile :: (Monad m) => (Char -> Bool) -> Pipe Text Text m ()
takeWhile predicate = go
  where
    go = do
        txt <- await
        let (prefix, suffix) = T.span predicate txt
        if (T.null suffix)
            then do
                yield txt
                go
            else yield prefix
{-# INLINABLE takeWhile #-}

-- | Drop characters until they fail the predicate
dropWhile :: (Monad m) => (Char -> Bool) -> Pipe Text Text m r
dropWhile predicate = go where
    go = do
        txt <- await
        case T.findIndex (not . predicate) txt of
            Nothing -> go
            Just i -> do
                yield (T.drop i txt)
                cat
{-# INLINABLE dropWhile #-}

-- | Only allows 'Char's to pass if they satisfy the predicate
filter :: (Monad m) => (Char -> Bool) -> Pipe Text Text m r
filter predicate = P.map (T.filter predicate)
{-# INLINABLE filter #-}

{-# RULES "p >-> filter q" forall p q .
        p >-> filter q = for p (\txt -> yield (T.filter q txt))
  #-}
  
-- | Strict left scan over the characters
scan
    :: (Monad m)
    => (Char -> Char -> Char) -> Char -> Pipe Text Text m r
scan step begin = do
    yield (T.singleton begin)
    go begin
  where
    go c = do
        txt <- await
        let txt' = T.scanl step c txt
            c' = T.last txt'
        yield (T.tail txt')
        go c'
{-# INLINABLE scan #-}

{-| Fold a pure 'Producer' of strict 'Text's into a lazy
    'TL.Text'
-}
toLazy :: Producer Text Identity () -> TL.Text
toLazy = TL.fromChunks . P.toList
{-# INLINABLE toLazy #-}

{-| Fold an effectful 'Producer' of strict 'Text's into a lazy
    'TL.Text'

    Note: 'toLazyM' is not an idiomatic use of @pipes@, but I provide it for
    simple testing purposes.  Idiomatic @pipes@ style consumes the chunks
    immediately as they are generated instead of loading them all into memory.
-}
toLazyM :: (Monad m) => Producer Text m () -> m TL.Text
toLazyM = liftM TL.fromChunks . P.toListM
{-# INLINABLE toLazyM #-}

-- | Reduce the text stream using a strict left fold over characters
foldChars
    :: Monad m
    => (x -> Char -> x) -> x -> (x -> r) -> Producer Text m () -> m r
foldChars step begin done = P.fold (T.foldl' step) begin done
{-# INLINABLE foldChars #-}

-- | Retrieve the first 'Char'
head :: (Monad m) => Producer Text m () -> m (Maybe Char)
head = go
  where
    go p = do
        x <- nextChar p
        case x of
            Left   _      -> return  Nothing
            Right (c, _) -> return (Just c)
{-# INLINABLE head #-}

-- | Retrieve the last 'Char'
last :: (Monad m) => Producer Text m () -> m (Maybe Char)
last = go Nothing
  where
    go r p = do
        x <- next p
        case x of
            Left   ()      -> return r
            Right (txt, p') ->
                if (T.null txt)
                then go r p'
                else go (Just $ T.last txt) p'
{-# INLINABLE last #-}

-- | Determine if the stream is empty
null :: (Monad m) => Producer Text m () -> m Bool
null = P.all T.null
{-# INLINABLE null #-}

-- | Count the number of characters in the stream
length :: (Monad m, Num n) => Producer Text m () -> m n
length = P.fold (\n txt -> n + fromIntegral (T.length txt)) 0 id
{-# INLINABLE length #-}

-- | Fold that returns whether 'M.Any' received 'Char's satisfy the predicate
any :: (Monad m) => (Char -> Bool) -> Producer Text m () -> m Bool
any predicate = P.any (T.any predicate)
{-# INLINABLE any #-}

-- | Fold that returns whether 'M.All' received 'Char's satisfy the predicate
all :: (Monad m) => (Char -> Bool) -> Producer Text m () -> m Bool
all predicate = P.all (T.all predicate)
{-# INLINABLE all #-}

-- | Return the maximum 'Char' within a text stream
maximum :: (Monad m) => Producer Text m () -> m (Maybe Char)
maximum = P.fold step Nothing id
  where
    step mc txt =
        if (T.null txt)
        then mc
        else Just $ case mc of
            Nothing -> T.maximum txt
            Just c -> max c (T.maximum txt)
{-# INLINABLE maximum #-}

-- | Return the minimum 'Char' within a text stream (surely very useful!)
minimum :: (Monad m) => Producer Text m () -> m (Maybe Char)
minimum = P.fold step Nothing id
  where
    step mc txt =
        if (T.null txt)
        then mc
        else case mc of
            Nothing -> Just (T.minimum txt)
            Just c -> Just (min c (T.minimum txt))
{-# INLINABLE minimum #-}


-- | Find the first element in the stream that matches the predicate
find
    :: (Monad m)
    => (Char -> Bool) -> Producer Text m () -> m (Maybe Char)
find predicate p = head (p >-> filter predicate)
{-# INLINABLE find #-}

-- | Index into a text stream
index
    :: (Monad m, Integral a)
    => a-> Producer Text m () -> m (Maybe Char)
index n p = head (p >-> drop n)
{-# INLINABLE index #-}


-- | Store a tally of how many segments match the given 'Text'
count :: (Monad m, Num n) => Text -> Producer Text m () -> m n
count c p = P.fold (+) 0 id (p >-> P.map (fromIntegral . T.count c))
{-# INLINABLE count #-}


{-| Consume the first character from a stream of 'Text'

    'next' either fails with a 'Left' if the 'Producer' has no more characters or
    succeeds with a 'Right' providing the next character and the remainder of the
    'Producer'.
-}
nextChar
    :: (Monad m)
    => Producer Text m r
    -> m (Either r (Char, Producer Text m r))
nextChar = go
  where
    go p = do
        x <- next p
        case x of
            Left   r       -> return (Left r)
            Right (txt, p') -> case (T.uncons txt) of
                Nothing        -> go p'
                Just (c, txt') -> return (Right (c, yield txt' >> p'))
{-# INLINABLE nextChar #-}

{-| Draw one 'Char' from a stream of 'Text', returning 'Left' if the
    'Producer' is empty
-}
drawChar :: (Monad m) => Parser Text m (Maybe Char)
drawChar = do
    x <- PP.draw
    case x of
        Nothing  -> return Nothing
        Just txt -> case (T.uncons txt) of
            Nothing        -> drawChar
            Just (c, txt') -> do
                PP.unDraw txt'
                return (Just c)
{-# INLINABLE drawChar #-}

-- | Push back a 'Char' onto the underlying 'Producer'
unDrawChar :: (Monad m) => Char -> Parser Text m ()
unDrawChar c = modify (yield (T.singleton c) >>)
{-# INLINABLE unDrawChar #-}

{-| 'peekChar' checks the first 'Char' in the stream, but uses 'unDrawChar' to
    push the 'Char' back

> peekChar = do
>     x <- drawChar
>     case x of
>         Left  _  -> return ()
>         Right c -> unDrawChar c
>     return x
-}
peekChar :: (Monad m) => Parser Text m (Maybe Char)
peekChar = do
    x <- drawChar
    case x of
        Nothing  -> return ()
        Just c -> unDrawChar c
    return x
{-# INLINABLE peekChar #-}

{-| Check if the underlying 'Producer' has no more characters

    Note that this will skip over empty 'Text' chunks, unlike
    'PP.isEndOfInput' from @pipes-parse@, which would consider
    an empty 'Text' a valid bit of input.

> isEndOfChars = liftM isLeft peekChar
-}
isEndOfChars :: (Monad m) => Parser Text m Bool
isEndOfChars = do
    x <- peekChar
    return (case x of
        Nothing -> True
        Just _-> False )
{-# INLINABLE isEndOfChars #-}


{- | An improper lens into a stream of 'ByteString' expected to be UTF-8 encoded; the associated
   stream of Text ends by returning a stream of ByteStrings beginning at the point of failure. 
   -}

decodeUtf8 :: Monad m => Lens' (Producer ByteString m r) 
                               (Producer Text m (Producer ByteString m r))
decodeUtf8 k p0 = fmap (\p -> join  (for p (yield . TE.encodeUtf8))) 
                       (k (go B.empty PI.streamDecodeUtf8 p0)) where
  go !carry dec0 p = do 
     x <- lift (next p) 
     case x of Left r -> return (if B.null carry 
                                    then return r -- all bytestring input was consumed
                                    else (do yield carry -- a potentially valid fragment remains
                                             return r))
                                           
               Right (chunk, p') -> case dec0 chunk of 
                   PI.Some text carry2 dec -> do yield text
                                                 go carry2 dec p'
                   PI.Other text bs -> do yield text 
                                          return (do yield bs -- an invalid blob remains
                                                     p')
{-# INLINABLE decodeUtf8 #-}


-- | Splits a 'Producer' after the given number of characters
splitAt
    :: (Monad m, Integral n)
    => n
    -> Lens' (Producer Text m r)
             (Producer Text m (Producer Text m r))
splitAt n0 k p0 = fmap join (k (go n0 p0))
  where
    go 0 p = return p
    go n p = do
        x <- lift (next p)
        case x of
            Left   r       -> return (return r)
            Right (txt, p') -> do
                let len = fromIntegral (T.length txt)
                if (len <= n)
                    then do
                        yield txt
                        go (n - len) p'
                    else do
                        let (prefix, suffix) = T.splitAt (fromIntegral n) txt
                        yield prefix
                        return (yield suffix >> p')
{-# INLINABLE splitAt #-}


{-| Split a text stream in two, where the first text stream is the longest
    consecutive group of text that satisfy the predicate
-}
span
    :: (Monad m)
    => (Char -> Bool)
    -> Lens' (Producer Text m r)
             (Producer Text m (Producer Text m r))
span predicate k p0 = fmap join (k (go p0))
  where
    go p = do
        x <- lift (next p)
        case x of
            Left   r       -> return (return r)
            Right (txt, p') -> do
                let (prefix, suffix) = T.span predicate txt
                if (T.null suffix)
                    then do
                        yield txt
                        go p'
                    else do
                        yield prefix
                        return (yield suffix >> p')
{-# INLINABLE span #-}

{-| Split a text stream in two, where the first text stream is the longest
    consecutive group of characters that don't satisfy the predicate
-}
break
    :: (Monad m)
    => (Char -> Bool)
    -> Lens' (Producer Text m r)
             (Producer Text m (Producer Text m r))
break predicate = span (not . predicate)
{-# INLINABLE break #-}

{-| Improper lens that splits after the first group of equivalent Chars, as
    defined by the given equivalence relation
-}
groupBy
    :: (Monad m)
    => (Char -> Char -> Bool)
    -> Lens' (Producer Text m r)
             (Producer Text m (Producer Text m r))
groupBy equals k p0 = fmap join (k ((go p0))) where
    go p = do
        x <- lift (next p)
        case x of
            Left   r       -> return (return r)
            Right (txt, p') -> case T.uncons txt of
                Nothing      -> go p'
                Just (c, _) -> (yield txt >> p') ^. span (equals c) 
{-# INLINABLE groupBy #-}

-- | Improper lens that splits after the first succession of identical 'Char' s
group :: Monad m 
      => Lens' (Producer Text m r)
               (Producer Text m (Producer Text m r))
group = groupBy (==)
{-# INLINABLE group #-}

{-| Improper lens that splits a 'Producer' after the first word

    Unlike 'words', this does not drop leading whitespace 
-}
word :: (Monad m) 
     => Lens' (Producer Text m r)
              (Producer Text m (Producer Text m r))
word k p0 = fmap join (k (to p0))
  where
    to p = do
        p' <- p^.span isSpace
        p'^.break isSpace
{-# INLINABLE word #-}


line :: (Monad m) 
     => Lens' (Producer Text m r)
              (Producer Text m (Producer Text m r))
line = break (== '\n')

{-# INLINABLE line #-}


-- | Intersperse a 'Char' in between the characters of stream of 'Text'
intersperse
    :: (Monad m) => Char -> Producer Text m r -> Producer Text m r
intersperse c = go0
  where
    go0 p = do
        x <- lift (next p)
        case x of
            Left   r       -> return r
            Right (txt, p') -> do
                yield (T.intersperse c txt)
                go1 p'
    go1 p = do
        x <- lift (next p)
        case x of
            Left   r       -> return r
            Right (txt, p') -> do
                yield (T.singleton c)
                yield (T.intersperse c txt)
                go1 p'
{-# INLINABLE intersperse #-}



-- | Improper isomorphism between a 'Producer' of 'ByteString's and 'Word8's
packChars :: Monad m => Iso' (Producer Char m x) (Producer Text m x)
packChars = Data.Profunctor.dimap to (fmap from)
  where
    -- to :: Monad m => Producer Char m x -> Producer Text m x
    to p = PG.folds step id done (p^.PG.chunksOf defaultChunkSize)

    step diffAs c = diffAs . (c:)

    done diffAs = T.pack (diffAs [])

    -- from :: Monad m => Producer Text m x -> Producer Char m x
    from p = for p (each . T.unpack)
{-# INLINABLE packChars #-}


-- | Split a text stream into 'FreeT'-delimited text streams of fixed size
chunksOf
    :: (Monad m, Integral n)
    => n -> Lens' (Producer Text m r) 
                  (FreeT (Producer Text m) m r)
chunksOf n k p0 = fmap concats (k (FreeT (go p0)))
  where
    go p = do
        x <- next p
        return $ case x of
            Left   r       -> Pure r
            Right (txt, p') -> Free $ do
                p'' <- (yield txt >> p') ^. splitAt n 
                return $ FreeT (go p'')
{-# INLINABLE chunksOf #-}


{-| Split a text stream into sub-streams delimited by characters that satisfy the
    predicate
-}
splitsWith
    :: (Monad m)
    => (Char -> Bool)
    -> Producer Text m r
    -> FreeT (Producer Text m) m r
splitsWith predicate p0 = FreeT (go0 p0)
  where
    go0 p = do
        x <- next p
        case x of
            Left   r       -> return (Pure r)
            Right (txt, p') ->
                if (T.null txt)
                then go0 p'
                else return $ Free $ do
                    p'' <-  (yield txt >> p') ^. span (not . predicate)
                    return $ FreeT (go1 p'')
    go1 p = do
        x <- nextChar p
        return $ case x of
            Left   r      -> Pure r
            Right (_, p') -> Free $ do
                    p'' <- p' ^. span (not . predicate) 
                    return $ FreeT (go1 p'')
{-# INLINABLE splitsWith #-}

-- | Split a text stream using the given 'Char' as the delimiter
splits :: (Monad m)
      => Char
      -> Lens' (Producer Text m r)
               (FreeT (Producer Text m) m r)
splits c k p =
          fmap (PG.intercalates (yield (T.singleton c))) (k (splitsWith (c ==) p))
{-# INLINABLE splits #-}

{-| Isomorphism between a stream of 'Text' and groups of equivalent 'Char's , using the
    given equivalence relation
-}
groupsBy
    :: Monad m
    => (Char -> Char -> Bool)
    -> Lens' (Producer Text m x) (FreeT (Producer Text m) m x)
groupsBy equals k p0 = fmap concats (k (FreeT (go p0))) where 
  go p = do x <- next p
            case x of Left   r       -> return (Pure r)
                      Right (bs, p') -> case T.uncons bs of
                             Nothing      -> go p'
                             Just (c, _) -> do return $ Free $ do
                                                 p'' <- (yield bs >> p')^.span (equals c)
                                                 return $ FreeT (go p'')
{-# INLINABLE groupsBy #-}


-- | Like 'groupsBy', where the equality predicate is ('==')
groups
    :: Monad m
    => Lens' (Producer Text m x) (FreeT (Producer Text m) m x)
groups = groupsBy (==)
{-# INLINABLE groups #-}



{-| Split a text stream into 'FreeT'-delimited lines
-}
lines
    :: (Monad m) => Iso' (Producer Text m r)  (FreeT (Producer Text m) m r)
lines = Data.Profunctor.dimap _lines (fmap _unlines)
  where
  _lines p0 = FreeT (go0 p0) 
    where
      go0 p = do
              x <- next p
              case x of
                  Left   r       -> return (Pure r)
                  Right (txt, p') ->
                      if (T.null txt)
                      then go0 p'
                      else return $ Free $ go1 (yield txt >> p')
      go1 p = do
              p' <- p ^. break ('\n' ==)
              return $ FreeT $ do
                  x  <- nextChar p'
                  case x of
                      Left   r      -> return $ Pure r
                      Right (_, p'') -> go0 p''
  -- _unlines
  --     :: Monad m
  --      => FreeT (Producer Text m) m x -> Producer Text m x
  _unlines = concats . PG.maps (<* yield (T.singleton '\n'))
  

{-# INLINABLE lines #-}


-- | Split a text stream into 'FreeT'-delimited words
words
    :: (Monad m) => Iso' (Producer Text m r) (FreeT (Producer Text m) m r)
words = Data.Profunctor.dimap go (fmap _unwords)
  where
    go p = FreeT $ do
        x <- next (p >-> dropWhile isSpace)
        return $ case x of
            Left   r       -> Pure r
            Right (bs, p') -> Free $ do
                p'' <-  (yield bs >> p') ^. break isSpace
                return (go p'')
    _unwords = PG.intercalates (yield $ T.singleton ' ')
    
{-# INLINABLE words #-}


{-| 'intercalate' concatenates the 'FreeT'-delimited text streams after
    interspersing a text stream in between them
-}
intercalate
    :: (Monad m)
    => Producer Text m ()
    -> FreeT (Producer Text m) m r
    -> Producer Text m r
intercalate p0 = go0
  where
    go0 f = do
        x <- lift (runFreeT f)
        case x of
            Pure r -> return r
            Free p -> do
                f' <- p
                go1 f'
    go1 f = do
        x <- lift (runFreeT f)
        case x of
            Pure r -> return r
            Free p -> do
                p0
                f' <- p
                go1 f'
{-# INLINABLE intercalate #-}

{-| Join 'FreeT'-delimited lines into a text stream
-}
unlines
    :: (Monad m) => FreeT (Producer Text m) m r -> Producer Text m r
unlines = go
  where
    go f = do
        x <- lift (runFreeT f)
        case x of
            Pure r -> return r
            Free p -> do
                f' <- p
                yield $ T.singleton '\n'
                go f'
{-# INLINABLE unlines #-}

{-| Join 'FreeT'-delimited words into a text stream
-}
unwords
    :: (Monad m) => FreeT (Producer Text m) m r -> Producer Text m r
unwords = intercalate (yield $ T.singleton ' ')
{-# INLINABLE unwords #-}

{- $parse
    The following parsing utilities are single-character analogs of the ones found
    @pipes-parse@.
-}

{- $reexports
    
    @Data.Text@ re-exports the 'Text' type.

    @Pipes.Parse@ re-exports 'input', 'concat', 'FreeT' (the type) and the 'Parse' synonym. 
-}

{- | Use a 'Codec' as a pipes-style 'Lens' into a byte stream; the available 'Codec' s are
     'utf8', 'utf16_le', 'utf16_be', 'utf32_le', 'utf32_be' . The 'Codec' concept and the 
     individual 'Codec' definitions follow the enumerator and conduit libraries. 
     
     Utf8 is handled differently in this library -- without the use of 'unsafePerformIO' &co 
     to catch 'Text' exceptions; but the same 'mypipe ^. codec utf8' interface can be used.
     'mypipe ^. decodeUtf8' should be the same, but has a somewhat more direct and thus perhaps
     better implementation.  

     -}
codec :: Monad m => Codec -> Lens' (Producer ByteString m r) (Producer Text m (Producer ByteString m r))
codec (Codec _ enc dec) k p0 = fmap (\p -> join (for p (yield . fst . enc))) 
                                     (k (decoder (dec B.empty) p0) ) where 
  decoder :: Monad m => PI.Decoding -> Producer ByteString m r -> Producer Text m (Producer ByteString m r)
  decoder !d p0 = case d of 
      PI.Other txt bad      -> do yield txt
                                  return (do yield bad
                                             p0)
      PI.Some txt extra dec -> do yield txt
                                  x <- lift (next p0)
                                  case x of Left r -> return (do yield extra
                                                                 return r)
                                            Right (chunk,p1) -> decoder (dec chunk) p1

{- | ascii and latin encodings only represent a small fragment of 'Text'; thus we cannot
     use the pipes 'Lens' style to work with them. Rather we simply define functions 
     each way. 

     'encodeAscii' : Reduce as much of your stream of 'Text' actually is ascii to a byte stream,
     returning the rest of the 'Text' at the first non-ascii 'Char'
-}
encodeAscii :: Monad m => Producer Text m r -> Producer ByteString m (Producer Text m r)
encodeAscii = go where
  go p = do echunk <- lift (next p)
            case echunk of 
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
  go p = do etxt <- lift (next p)
            case etxt of 
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
  go p = do echunk <- lift (next p)
            case echunk of 
              Left r -> return (return r)
              Right (chunk, p') -> 
                 if B.null chunk 
                   then go p'
                   else let (safe, unsafe)  = B.span (<= 0x7F) chunk
                        in do yield (T.pack (B8.unpack safe))
                              if B.null unsafe
                                then go p'
                                else return $ do yield unsafe 
                                                 p'

{- | Reduce a byte stream to a corresponding stream of ascii chars, returning the
     unused 'ByteString' upon hitting the rare un-latinizable byte.
     -}
decodeIso8859_1 :: Monad m => Producer ByteString m r -> Producer Text m (Producer ByteString m r)
decodeIso8859_1 = go where
  go p = do echunk <- lift (next p)
            case echunk of 
              Left r -> return (return r)
              Right (chunk, p') -> 
                 if B.null chunk 
                   then go p'
                   else let (safe, unsafe)  = B.span (<= 0xFF) chunk
                        in do yield (T.pack (B8.unpack safe))
                              if B.null unsafe
                                then go p'
                                else return $ do yield unsafe 
                                                 p'




                                            
