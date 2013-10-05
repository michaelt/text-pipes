{-# LANGUAGE RankNTypes, TypeFamilies #-}

{-| This module provides @pipes@ utilities for \"byte streams\", which are
    streams of strict 'Text's chunks.  Use byte streams to interact
    with both 'IO.Handle's and lazy 'Text's.

    To stream to or from 'IO.Handle's, use 'fromHandle' or 'toHandle'.  For
    example, the following program copies data from one file to another:

> import Pipes
> import qualified Pipes.Text as P
> import System.IO
>
> main =
>     withFile "inFile.txt"  ReadMode  $ \hIn  ->
>     withFile "outFile.txt" WriteMode $ \hOut ->
>     runEffect $ P.fromHandle hIn >-> P.toHandle hOut

    You can stream to and from 'stdin' and 'stdout' using the predefined 'stdin'
    and 'stdout' proxies, like in the following \"echo\" program:

> main = runEffect $ P.stdin >-> P.stdout

    You can also translate pure lazy 'TL.Text's to and from proxies:

> import qualified Data.Text.Lazy as TL
>
> main = runEffect $ P.fromLazy (TL.pack "Hello, world!\n") >-> P.stdout

    In addition, this module provides many functions equivalent to lazy
    'Text' functions so that you can transform or fold byte streams.  For
    example, to stream only the first three lines of 'stdin' to 'stdout' you
    would write:

> import Pipes
> import qualified Pipes.Text as PT
> import qualified Pipes.Parse      as PP
>
> main = runEffect $ takeLines 3 PB.stdin >-> PT.stdout
>   where
>     takeLines n = PB.unlines . PP.takeFree n . PT.lines

    The above program will never bring more than one chunk (~ 32 KB) into
    memory, no matter how long the lines are.

    Note that functions in this library are designed to operate on streams that
    are insensitive to chunk boundaries.  This means that they may freely split
    chunks into smaller chunks and /discard empty chunks/.  However, they will
    /never concatenate chunks/ in order to provide strict upper bounds on memory
    usage.
-}

module Data.Text.Pipes  (
    -- * Producers
    fromLazy,
    stdin,
    fromHandle,
    readFile,
    stdinLn,
-- hGetSome,
-- hGet,

    -- * Servers
-- hGetSomeN,
-- hGetN,

    -- * Consumers
    stdout,
    stdoutLn,
    toHandle,
    writeFile,

    -- * Pipes
    map,
    concatMap,
    take,
    drop,
    takeWhile,
    dropWhile,
    filter,
--    elemIndices,
--    findIndices,
    scan,

    -- * Folds
    toLazy,
    toLazyM,
    fold,
    head,
    last,
    null,
    length,
    any,
    all,
    maximum,
    minimum,
--    elem,
--    notElem,
    find,
    index,
--    elemIndex,
--    findIndex,
--    count,

    -- * Splitters
    splitAt,
    chunksOf,
    span,
    break,
    splitWith,
    split,
    groupBy,
    group,
    lines,
    words,

    -- * Transformations
    intersperse,

    -- * Joiners
    intercalate,
    unlines,
    unwords,

    -- * Low-level Parsers
    -- $parse
    nextByte,
    drawByte,
    unDrawByte,
    peekByte,
    isEndOfBytes,
--    takeWhile',

    -- * Re-exports
    -- $reexports
    module Data.Text,
--    module Data.Word,
    module Pipes.Parse
    ) where

import Control.Exception (throwIO, try)
import Control.Monad (liftM, unless)
import Control.Monad.Trans.State.Strict (StateT)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Text.Lazy.Internal (foldrChunks, defaultChunkSize)
import Data.ByteString.Unsafe (unsafeTake, unsafeDrop)
import Data.Char (ord)
import Data.Functor.Identity (Identity)
import qualified Data.List as List
import Foreign.C.Error (Errno(Errno), ePIPE)
import qualified GHC.IO.Exception as G
import Pipes
import qualified Pipes.ByteString.Parse as PBP
import Pipes.ByteString.Parse (
    nextByte, drawByte, unDrawByte, peekByte, isEndOfBytes )
import Pipes.Core (respond, Server')
import qualified Pipes.Parse as PP
import Pipes.Parse (input, concat, FreeT)
import qualified Pipes.Safe.Prelude as Safe
import qualified Pipes.Safe as Safe
import Pipes.Safe (MonadSafe(..), Base(..))
import qualified Pipes.Prelude as P
import qualified System.IO as IO
import Data.Char (isSpace)
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
{-# INLINABLE fromLazy #-}

-- | Stream bytes from 'stdin'
stdin :: MonadIO m => Producer' Text m ()
stdin = fromHandle IO.stdin
{-# INLINABLE stdin #-}

-- | Convert a 'IO.Handle' into a byte stream using a default chunk size
fromHandle :: MonadIO m => IO.Handle -> Producer' Text m ()
fromHandle h = go where
    go = do txt <- liftIO (T.hGetChunk h)
            unless (T.null txt) $ do yield txt
                                     go
{-# INLINABLE fromHandle#-}

readFile :: (MonadSafe m, Base m ~ IO) => FilePath -> Producer' Text m ()
readFile file = Safe.withFile file IO.ReadMode fromHandle
{-# INLINABLE readFile #-}

stdinLn :: MonadIO m => Producer' Text m ()
stdinLn = go
  where
    go = do
        eof <- liftIO (IO.hIsEOF IO.stdin)
        unless eof $ do
            txt <- liftIO (T.hGetLine IO.stdin)
            yield txt
            go

{-| Convert a handle into a byte stream using a fixed chunk size

    'hGet' waits until exactly the requested number of bytes are available for
    each chunk.
-}
-- hGet :: MonadIO m => Int -> IO.Handle -> Producer' Text m ()
-- hGet size h = go where
--     go = do
--         eof <- liftIO (IO.hIsEOF h)
--         if eof
--             then return ()
--             else do
--                 bs <- liftIO (T.hGet h size)
--                 yield bs
--                 go
-- {-# INLINABLE hGet #-}

{-| Like 'hGetSome', except you can vary the maximum chunk size for each request
-}
-- hGetSomeN :: MonadIO m => IO.Handle -> Int -> Server' Int Text m ()
-- hGetSomeN h = go where
--     go size = do
--         eof <- liftIO (IO.hIsEOF h)
--         if eof
--             then return ()
--             else do
--                 bs    <- liftIO (T.hGetSome h size)
--                 size2 <- respond bs
--                 go size2
-- {-# INLINABLE hGetSomeN #-}
-- 
-- -- | Like 'hGet', except you can vary the chunk size for each request
-- hGetN :: MonadIO m => IO.Handle -> Int -> Server' Int Text m ()
-- hGetN h = go where
--     go size = do
--         eof <- liftIO (IO.hIsEOF h)
--         if eof
--             then return ()
--             else do
--                 bs    <- liftIO (T.hGet h size)
--                 size2 <- respond bs
--                 go size2
-- {-# INLINABLE hGetN #-}

{-| Stream bytes to 'stdout'

    Unlike 'toHandle', 'stdout' gracefully terminates on a broken output pipe.

    Note: For best performance, use @(for source (liftIO . putStr))@ instead of
    @(source >-> stdout)@.
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

stdoutLn :: (MonadIO m) => Consumer' Text m ()
stdoutLn = go
  where
    go = do
        str <- await
        x   <- liftIO $ try (T.putStrLn str)
        case x of
           Left (G.IOError { G.ioe_type  = G.ResourceVanished
                           , G.ioe_errno = Just ioe })
                | Errno ioe == ePIPE
                    -> return ()
           Left  e  -> liftIO (throwIO e)
           Right () -> go
{-# INLINABLE stdoutLn #-}

{-| Convert a byte stream into a 'Handle'

    Note: For best performance, use @(for source (liftIO . hPutStr handle))@
    instead of @(source >-> toHandle handle)@.
-}
toHandle :: MonadIO m => IO.Handle -> Consumer' Text m r
toHandle h = for cat (liftIO . T.hPutStr h)
{-# INLINABLE toHandle #-}

writeFile :: (MonadSafe m, Base m ~ IO) => FilePath -> Consumer' Text m ()
writeFile file = Safe.withFile file IO.WriteMode toHandle

-- | Apply a transformation to each 'Char' in the stream
map :: (Monad m) => (Char -> Char) -> Pipe Text Text m r
map f = P.map (T.map f)
{-# INLINABLE map #-}

-- | Map a function over the byte stream and concatenate the results
concatMap
    :: (Monad m) => (Char -> Text) -> Pipe Text Text m r
concatMap f = P.map (T.concatMap f)
{-# INLINABLE concatMap #-}

-- | @(take n)@ only allows @n@ bytes to pass
take :: (Monad m, Integral a) => a -> Pipe Text Text m ()
take n0 = go n0 where
    go n
        | n <= 0    = return ()
        | otherwise = do
            bs <- await
            let len = fromIntegral (T.length bs)
            if (len > n)
                then yield (T.take (fromIntegral n) bs)
                else do
                    yield bs
                    go (n - len)
{-# INLINABLE take #-}

-- | @(dropD n)@ drops the first @n@ bytes
drop :: (Monad m, Integral a) => a -> Pipe Text Text m r
drop n0 = go n0 where
    go n
        | n <= 0    = cat
        | otherwise = do
            bs <- await
            let len = fromIntegral (T.length bs)
            if (len >= n)
                then do
                    yield (T.drop (fromIntegral n) bs)
                    cat
                else go (n - len)
{-# INLINABLE drop #-}

-- | Take bytes until they fail the predicate
takeWhile :: (Monad m) => (Char -> Bool) -> Pipe Text Text m ()
takeWhile predicate = go
  where
    go = do
        bs <- await
        let (prefix, suffix) = T.span predicate bs
        if (T.null suffix)
            then do
                yield bs
                go
            else yield prefix
{-# INLINABLE takeWhile #-}

-- | Drop bytes until they fail the predicate
dropWhile :: (Monad m) => (Char -> Bool) -> Pipe Text Text m r
dropWhile predicate = go where
    go = do
        bs <- await
        case T.findIndex (not . predicate) bs of
            Nothing -> go
            Just i -> do
                yield (T.drop i bs)
                cat
{-# INLINABLE dropWhile #-}

-- | Only allows 'Char's to pass if they satisfy the predicate
filter :: (Monad m) => (Char -> Bool) -> Pipe Text Text m r
filter predicate = P.map (T.filter predicate)
{-# INLINABLE filter #-}

-- | Stream all indices whose elements match the given 'Char'
-- elemIndices :: (Monad m, Num n) => Char -> Pipe Text n m r
-- elemIndices w8 = findIndices (w8 ==)
-- {-# INLINABLE elemIndices #-}

-- | Stream all indices whose elements satisfy the given predicate
-- findIndices :: (Monad m, Num n) => (Char -> Bool) -> Pipe Text n m r
-- findIndices predicate = go 0
--   where
--     go n = do
--         bs <- await
--  each $ List.map (\i -> n + fromIntegral i) (T.findIndices predicate bs)
--         go $! n + fromIntegral (T.length bs)
-- {-# INLINABLE findIndices #-}

-- | Strict left scan over the bytes
scan
    :: (Monad m)
    => (Char -> Char -> Char) -> Char -> Pipe Text Text m r
scan step begin = go begin
  where
    go w8 = do
        bs <- await
        let bs' = T.scanl step w8 bs
            w8' = T.last bs'
        yield bs'
        go w8'
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

-- | Reduce the stream of bytes using a strict left fold
fold
    :: Monad m
    => (x -> Char -> x) -> x -> (x -> r) -> Producer Text m () -> m r
fold step begin done = P.fold (\x bs -> T.foldl' step x bs) begin done
{-# INLINABLE fold #-}

-- | Retrieve the first 'Char'
head :: (Monad m) => Producer Text m () -> m (Maybe Char)
head = go
  where
    go p = do
        x <- nextChar p
        case x of
            Left   _      -> return  Nothing
            Right (w8, _) -> return (Just w8)
{-# INLINABLE head #-}

-- | Retrieve the last 'Char'
last :: (Monad m) => Producer Text m () -> m (Maybe Char)
last = go Nothing
  where
    go r p = do
        x <- next p
        case x of
            Left   ()      -> return r
            Right (bs, p') ->
                if (T.null bs)
                then go r p'
                else go (Just $ T.last bs) p'
                -- TODO: Change this to 'unsafeLast' when bytestring-0.10.2.0
                --       becomes more widespread
{-# INLINABLE last #-}

-- | Determine if the stream is empty
null :: (Monad m) => Producer Text m () -> m Bool
null = P.all T.null
{-# INLINABLE null #-}

-- | Count the number of bytes
length :: (Monad m, Num n) => Producer Text m () -> m n
length = P.fold (\n bs -> n + fromIntegral (T.length bs)) 0 id
{-# INLINABLE length #-}

-- | Fold that returns whether 'M.Any' received 'Char's satisfy the predicate
any :: (Monad m) => (Char -> Bool) -> Producer Text m () -> m Bool
any predicate = P.any (T.any predicate)
{-# INLINABLE any #-}

-- | Fold that returns whether 'M.All' received 'Char's satisfy the predicate
all :: (Monad m) => (Char -> Bool) -> Producer Text m () -> m Bool
all predicate = P.all (T.all predicate)
{-# INLINABLE all #-}

-- | Return the maximum 'Char' within a byte stream
maximum :: (Monad m) => Producer Text m () -> m (Maybe Char)
maximum = P.fold step Nothing id
  where
    step mw8 bs =
        if (T.null bs)
        then mw8
        else Just $ case mw8 of
            Nothing -> T.maximum bs
            Just w8 -> max w8 (T.maximum bs)
{-# INLINABLE maximum #-}

-- | Return the minimum 'Char' within a byte stream
minimum :: (Monad m) => Producer Text m () -> m (Maybe Char)
minimum = P.fold step Nothing id
  where
    step mw8 bs =
        if (T.null bs)
        then mw8
        else case mw8 of
            Nothing -> Just (T.minimum bs)
            Just w8 -> Just (min w8 (T.minimum bs))
{-# INLINABLE minimum #-}

-- | Determine whether any element in the byte stream matches the given 'Char'
-- elem :: (Monad m) => Char -> Producer Text m () -> m Bool
-- elem w8 = P.any (T.elem w8)
-- {-# INLINABLE elem #-}
-- 
-- {-| Determine whether all elements in the byte stream do not match the given
--     'Char'
-- -}
-- notElem :: (Monad m) => Char -> Producer Text m () -> m Bool
-- notElem w8 = P.all (T.notElem w8)
-- {-# INLINABLE notElem #-}

-- | Find the first element in the stream that matches the predicate
find
    :: (Monad m)
    => (Char -> Bool) -> Producer Text m () -> m (Maybe Char)
find predicate p = head (p >-> filter predicate)
{-# INLINABLE find #-}

-- | Index into a byte stream
index
    :: (Monad m, Integral a)
    => a-> Producer Text m () -> m (Maybe Char)
index n p = head (p >-> drop n)
{-# INLINABLE index #-}

-- | Find the index of an element that matches the given 'Char'
-- elemIndex
--     :: (Monad m, Num n) => Char -> Producer Text m () -> m (Maybe n)
-- elemIndex w8 = findIndex (w8 ==)
-- {-# INLINABLE elemIndex #-}

-- | Store the first index of an element that satisfies the predicate
-- findIndex
--     :: (Monad m, Num n)
--     => (Char -> Bool) -> Producer Text m () -> m (Maybe n)
-- findIndex predicate p = P.head (p >-> findIndices predicate)
-- {-# INLINABLE findIndex #-}
-- 
-- -- | Store a tally of how many elements match the given 'Char'
-- count :: (Monad m, Num n) => Char -> Producer Text m () -> m n
-- count w8 p = P.fold (+) 0 id (p >-> P.map (fromIntegral . T.count w8))
-- {-# INLINABLE count #-}

-- | Splits a 'Producer' after the given number of bytes
splitAt
    :: (Monad m, Integral n)
    => n
    -> Producer Text m r
    -> Producer' Text m (Producer Text m r)
splitAt = go
  where
    go 0 p = return p
    go n p = do
        x <- lift (next p)
        case x of
            Left   r       -> return (return r)
            Right (bs, p') -> do
                let len = fromIntegral (T.length bs)
                if (len <= n)
                    then do
                        yield bs
                        go (n - len) p'
                    else do
                        let (prefix, suffix) = T.splitAt (fromIntegral n) bs
                        yield prefix
                        return (yield suffix >> p')
{-# INLINABLE splitAt #-}

-- | Split a byte stream into 'FreeT'-delimited byte streams of fixed size
chunksOf
    :: (Monad m, Integral n)
    => n -> Producer Text m r -> FreeT (Producer Text m) m r
chunksOf n p0 = PP.FreeT (go p0)
  where
    go p = do
        x <- next p
        return $ case x of
            Left   r       -> PP.Pure r
            Right (bs, p') -> PP.Free $ do
                p'' <- splitAt n (yield bs >> p')
                return $ PP.FreeT (go p'')
{-# INLINABLE chunksOf #-}

{-| Split a byte stream in two, where the first byte stream is the longest
    consecutive group of bytes that satisfy the predicate
-}
span
    :: (Monad m)
    => (Char -> Bool)
    -> Producer Text m r
    -> Producer' Text m (Producer Text m r)
span predicate = go
  where
    go p = do
        x <- lift (next p)
        case x of
            Left   r       -> return (return r)
            Right (bs, p') -> do
                let (prefix, suffix) = T.span predicate bs
                if (T.null suffix)
                    then do
                        yield bs
                        go p'
                    else do
                        yield prefix
                        return (yield suffix >> p')
{-# INLINABLE span #-}

{-| Split a byte stream in two, where the first byte stream is the longest
    consecutive group of bytes that don't satisfy the predicate
-}
break
    :: (Monad m)
    => (Char -> Bool)
    -> Producer Text m r
    -> Producer Text m (Producer Text m r)
break predicate = span (not . predicate)
{-# INLINABLE break #-}

{-| Split a byte stream into sub-streams delimited by bytes that satisfy the
    predicate
-}
splitWith
    :: (Monad m)
    => (Char -> Bool)
    -> Producer Text m r
    -> PP.FreeT (Producer Text m) m r
splitWith predicate p0 = PP.FreeT (go0 p0)
  where
    go0 p = do
        x <- next p
        case x of
            Left   r       -> return (PP.Pure r)
            Right (bs, p') ->
                if (T.null bs)
                then go0 p'
                else return $ PP.Free $ do
                    p'' <- span (not . predicate) (yield bs >> p')
                    return $ PP.FreeT (go1 p'')
    go1 p = do
        x <- nextChar p
        return $ case x of
            Left   r      -> PP.Pure r
            Right (_, p') -> PP.Free $ do
                    p'' <- span (not . predicate) p'
                    return $ PP.FreeT (go1 p'')
{-# INLINABLE splitWith #-}

-- | Split a byte stream using the given 'Char' as the delimiter
split :: (Monad m)
      => Char
      -> Producer Text m r
      -> FreeT (Producer Text m) m r
split w8 = splitWith (w8 ==)
{-# INLINABLE split #-}

{-| Group a byte stream into 'FreeT'-delimited byte streams using the supplied
    equality predicate
-}
groupBy
    :: (Monad m)
    => (Char -> Char -> Bool)
    -> Producer Text m r
    -> FreeT (Producer Text m) m r
groupBy equal p0 = PP.FreeT (go p0)
  where
    go p = do
        x <- next p
        case x of
            Left   r       -> return (PP.Pure r)
            Right (bs, p') -> case (T.uncons bs) of
                Nothing      -> go p'
                Just (w8, _) -> do
                    return $ PP.Free $ do
                        p'' <- span (equal w8) (yield bs >> p')
                        return $ PP.FreeT (go p'')
{-# INLINABLE groupBy #-}

-- | Group a byte stream into 'FreeT'-delimited byte streams of identical bytes
group
    :: (Monad m) => Producer Text m r -> FreeT (Producer Text m) m r
group = groupBy (==)
{-# INLINABLE group #-}

{-| Split a byte stream into 'FreeT'-delimited lines

    Note: This function is purely for demonstration purposes since it assumes a
    particular encoding.  You should prefer the 'Data.Text.Text' equivalent of
    this function from the upcoming @pipes-text@ library.
-}
lines
    :: (Monad m) => Producer Text m r -> FreeT (Producer Text m) m r
lines p0 = PP.FreeT (go0 p0)
  where
    go0 p = do
        x <- next p
        case x of
            Left   r       -> return (PP.Pure r)
            Right (bs, p') ->
                if (T.null bs)
                then go0 p'
                else return $ PP.Free $ go1 (yield bs >> p')
    go1 p = do
        p' <- break ('\n' ==) p
        return $ PP.FreeT (go2 p')
    go2 p = do
        x  <- nextChar p
        return $ case x of
            Left   r      -> PP.Pure r
            Right (_, p') -> PP.Free (go1 p')
{-# INLINABLE lines #-}
nextChar = undefined
{-| Split a byte stream into 'FreeT'-delimited words

    Note: This function is purely for demonstration purposes since it assumes a
    particular encoding.  You should prefer the 'Data.Text.Text' equivalent of
    this function from the upcoming @pipes-text@ library.
-}
words
    :: (Monad m) => Producer Text m r -> FreeT (Producer Text m) m r
words p0 = removeEmpty (splitWith isSpace p0)
  where
    removeEmpty f = PP.FreeT $ do
        x <- PP.runFreeT f
        case x of
            PP.Pure r -> return (PP.Pure r)
            PP.Free p -> do
                y <- next p
                case y of
                    Left   f'      -> PP.runFreeT (removeEmpty f')
                    Right (bs, p') -> return $ PP.Free $ do
                        yield bs
                        f' <- p'
                        return (removeEmpty f')
{-# INLINABLE words #-}

-- | Intersperse a 'Char' in between the bytes of the byte stream
intersperse
    :: (Monad m) => Char -> Producer Text m r -> Producer Text m r
intersperse w8 = go0
  where
    go0 p = do
        x <- lift (next p)
        case x of
            Left   r       -> return r
            Right (bs, p') -> do
                yield (T.intersperse w8 bs)
                go1 p'
    go1 p = do
        x <- lift (next p)
        case x of
            Left   r       -> return r
            Right (bs, p') -> do
                yield (T.singleton w8)
                yield (T.intersperse w8 bs)
                go1 p'
{-# INLINABLE intersperse #-}

{-| 'intercalate' concatenates the 'FreeT'-delimited byte streams after
    interspersing a byte stream in between them
-}
intercalate
    :: (Monad m)
    => Producer Text m ()
    -> FreeT (Producer Text m) m r
    -> Producer Text m r
intercalate p0 = go0
  where
    go0 f = do
        x <- lift (PP.runFreeT f)
        case x of
            PP.Pure r -> return r
            PP.Free p -> do
                f' <- p
                go1 f'
    go1 f = do
        x <- lift (PP.runFreeT f)
        case x of
            PP.Pure r -> return r
            PP.Free p -> do
                p0
                f' <- p
                go1 f'
{-# INLINABLE intercalate #-}

{-| Join 'FreeT'-delimited lines into a byte stream

    Note: This function is purely for demonstration purposes since it assumes a
    particular encoding.  You should prefer the 'Data.Text.Text' equivalent of
    this function from the upcoming @pipes-text@ library.
-}
unlines
    :: (Monad m) => FreeT (Producer Text m) m r -> Producer Text m r
unlines = go
  where
    go f = do
        x <- lift (PP.runFreeT f)
        case x of
            PP.Pure r -> return r
            PP.Free p -> do
                f' <- p
                yield $ T.singleton '\n'
                go f'
{-# INLINABLE unlines #-}

{-| Join 'FreeT'-delimited words into a byte stream

    Note: This function is purely for demonstration purposes since it assumes a
    particular encoding.  You should prefer the 'Data.Text.Text' equivalent of
    this function from the upcoming @pipes-text@ library.
-}
unwords
    :: (Monad m) => FreeT (Producer Text m) m r -> Producer Text m r
unwords = intercalate (yield $ T.pack " ")
{-# INLINABLE unwords #-}

{- $parse
    The following parsing utilities are single-byte analogs of the ones found
    in @pipes-parse@.
-}

{-| Take bytes until they fail the predicate

    Unlike 'takeWhile', this 'PP.unDraw's unused bytes
-}
-- takeWhile'
--     :: (Monad m)
--     => (Char -> Bool)
--     -> Pipe Text Text (StateT (Producer Text m r) m) ()
-- takeWhile' = PBP.takeWhile
-- {-# INLINABLE takeWhile' #-}
-- {-# DEPRECATED takeWhile' "Use Pipes.Text.Parse.takeWhile instead" #-}

{- $reexports
    "Pipes.Text.Parse" re-exports 'nextByte', 'drawByte', 'unDrawByte',
    'peekByte', and 'isEndOfBytes'.
    
    @Data.Text@ re-exports the 'Text' type.

    @Data.Word@ re-exports the 'Char' type.

    @Pipes.Parse@ re-exports 'input', 'concat', and 'FreeT' (the type).
-}