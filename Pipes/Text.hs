{-# LANGUAGE RankNTypes, TypeFamilies, BangPatterns, Trustworthy #-}

{-| The module @Pipes.Text@ closely follows @Pipes.ByteString@ from 
    the @pipes-bytestring@ package. A draft tutorial can be found in
    @Pipes.Text.Tutorial@. 
-}

module Pipes.Text  (
    -- * Producers
    fromLazy

    -- * Pipes
    , map
    , concatMap
    , take
    , takeWhile
    , filter
    , toCaseFold
    , toLower
    , toUpper
    , stripStart
    , scan

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

    -- * Primitive Character Parsers
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

    -- * Transforming Text and Character Streams
    , drop
    , dropWhile
    , pack
    , unpack
    , intersperse

    -- * FreeT Transformations
    , chunksOf
    , splitsWith
    , splits
    , groupsBy
    , groups
    , lines
    , unlines
    , words
    , unwords
    , intercalate

    -- * Re-exports
    -- $reexports
    , module Data.ByteString
    , module Data.Text
    , module Pipes.Parse
    , module Pipes.Group
    ) where

import Control.Applicative ((<*))
import Control.Monad (liftM, join)
import Control.Monad.Trans.State.Strict (StateT(..), modify)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.ByteString (ByteString)
import Data.Functor.Constant (Constant(Constant, getConstant))
import Data.Functor.Identity (Identity)

import Pipes
import Pipes.Group (folds, maps, concats, intercalates, FreeT(..), FreeF(..))
import qualified Pipes.Group as PG
import qualified Pipes.Parse as PP
import Pipes.Parse (Parser)
import qualified Pipes.Prelude as P
import Data.Char (isSpace)
import Data.Word (Word8)
import Foreign.Storable (sizeOf)
import Data.Bits (shiftL)
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
fromLazy  = TL.foldrChunks (\e a -> yield e >> a) (return ())
{-# INLINE fromLazy #-}

(^.) :: a -> ((b -> Constant b b) -> (a -> Constant b a)) -> b
a ^. lens = getConstant (lens Constant a)

-- | Apply a transformation to each 'Char' in the stream
map :: (Monad m) => (Char -> Char) -> Pipe Text Text m r
map f = P.map (T.map f)
{-# INLINABLE map #-}

-- | Map a function over the characters of a text stream and concatenate the results
concatMap
    :: (Monad m) => (Char -> Text) -> Pipe Text Text m r
concatMap f = P.map (T.concatMap f)
{-# INLINABLE concatMap #-}

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

-- | Only allows 'Char's to pass if they satisfy the predicate
filter :: (Monad m) => (Char -> Bool) -> Pipe Text Text m r
filter predicate = P.map (T.filter predicate)
{-# INLINABLE filter #-}

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

-- | @toCaseFold@, @toLower@, @toUpper@ and @stripStart@ are standard 'Text' utilities,
-- here acting as 'Text' pipes, rather as they would  on a lazy text
toCaseFold :: Monad m => Pipe Text Text m r
toCaseFold = P.map T.toCaseFold
{-# INLINEABLE toCaseFold #-}

-- | lowercase incoming 'Text'
toLower :: Monad m => Pipe Text Text m r
toLower = P.map T.toLower
{-# INLINEABLE toLower #-}

-- | uppercase incoming 'Text'
toUpper :: Monad m => Pipe Text Text m r
toUpper = P.map T.toUpper
{-# INLINEABLE toUpper #-}

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
index n p = head (drop n p)
{-# INLINABLE index #-}



-- | Consume the first character from a stream of 'Text'
--
-- 'next' either fails with a 'Left' if the 'Producer' has no more characters or
-- succeeds with a 'Right' providing the next character and the remainder of the
-- 'Producer'.

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

-- | Draw one 'Char' from a stream of 'Text', returning 'Left' if the 'Producer' is empty

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


-- | Split a text stream in two, producing the longest
--   consecutive group of characters that satisfies the predicate
--   and returning the rest

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

{-| Split a text stream in two, producing the longest
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

-- | @(drop n)@ drops the first @n@ characters
drop :: (Monad m, Integral n)
     => n -> Producer Text m r -> Producer Text m r
drop n p = do
    p' <- lift $ runEffect (for (p ^. splitAt n) discard)
    p'
{-# INLINABLE drop #-}

-- | Drop characters until they fail the predicate
dropWhile :: (Monad m)
    => (Char -> Bool) -> Producer Text m r -> Producer Text m r
dropWhile predicate p = do
    p' <- lift $ runEffect (for (p ^. span predicate) discard)
    p'
{-# INLINABLE dropWhile #-}

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


-- | Improper lens from unpacked 'Word8's to packaged 'ByteString's
pack :: Monad m => Lens' (Producer Char m r) (Producer Text m r)
pack k p = fmap _unpack (k (_pack p))
{-# INLINABLE pack #-}

-- | Improper lens from packed 'ByteString's to unpacked 'Word8's
unpack :: Monad m => Lens' (Producer Text m r) (Producer Char m r)
unpack k p = fmap _pack (k (_unpack p))
{-# INLINABLE unpack #-}

_pack :: Monad m => Producer Char m r -> Producer Text m r
_pack p = folds step id done (p^.PG.chunksOf defaultChunkSize)
  where
    step diffAs w8 = diffAs . (w8:)

    done diffAs = T.pack (diffAs [])
{-# INLINABLE _pack #-}

_unpack :: Monad m => Producer Text m r -> Producer Char m r
_unpack p = for p (each . T.unpack)
{-# INLINABLE _unpack #-}

defaultChunkSize :: Int
defaultChunkSize = 16384 - (sizeOf (undefined :: Int) `shiftL` 1)


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
    -> Producer Text m r -> FreeT (Producer Text m) m r
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
          fmap (intercalates (yield (T.singleton c))) (k (splitsWith (c ==) p))
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
    :: (Monad m) => Lens' (Producer Text m r)  (FreeT (Producer Text m) m r)
lines k p = fmap _unlines (k (_lines p))
{-# INLINABLE lines #-}

unlines
    :: Monad m
    => Lens' (FreeT (Producer Text m) m r) (Producer Text m r)
unlines k p = fmap _lines (k (_unlines p))
{-# INLINABLE unlines #-}

_lines :: Monad m
             => Producer Text m r -> FreeT (Producer Text m) m r
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
{-# INLINABLE _lines #-}

_unlines :: Monad m
         => FreeT (Producer Text m) m r -> Producer Text m r
_unlines = concats . maps (<* yield (T.singleton '\n'))
{-# INLINABLE _unlines #-}

-- | Split a text stream into 'FreeT'-delimited words. Note that 
-- roundtripping with e.g. @over words id@ eliminates extra space
-- characters as with @Prelude.unwords . Prelude.words@
words
    :: (Monad m) => Lens' (Producer Text m r) (FreeT (Producer Text m) m r)
words k p = fmap _unwords (k (_words p))
{-# INLINABLE words #-}

unwords
    :: Monad m
    => Lens' (FreeT (Producer Text m) m r) (Producer Text m r)
unwords k p = fmap _words (k (_unwords p))
{-# INLINABLE unwords #-}

_words :: (Monad m) => Producer Text m r -> FreeT (Producer Text m) m r
_words p = FreeT $ do
        x <- next (dropWhile isSpace p)
        return $ case x of
            Left   r       -> Pure r
            Right (bs, p') -> Free $ do
                p'' <-  (yield bs >> p') ^. break isSpace
                return (_words p'')
{-# INLINABLE _words #-}

_unwords :: (Monad m) => FreeT (Producer Text m) m r -> Producer Text m r
_unwords = intercalates (yield $ T.singleton ' ')
{-# INLINABLE _unwords #-}


{-| 'intercalate' concatenates the 'FreeT'-delimited text streams after
    interspersing a text stream in between them
-}
intercalate
    :: (Monad m)
    => Producer Text m () -> FreeT (Producer Text m) m r -> Producer Text m r
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



{- $reexports

    @Data.Text@ re-exports the 'Text' type.

    @Pipes.Parse@ re-exports 'input', 'concat', 'FreeT' (the type) and the 'Parse' synonym.
-}


type Lens' a b = forall f . Functor f => (b -> f b) -> (a -> f a)
