-- | Parsing utilities for texts, in the style of @pipes-parse@ and @Pipes.ByteString.Parse@

module Pipes.Text.Parse (
    -- * Parsers
    nextChar,
    drawChar,
    unDrawChar,
    peekChar,
    isEndOfChars,
    take,
    takeWhile
    ) where

import Control.Monad.Trans.State.Strict (StateT, modify)
import qualified Data.Text as T
import Data.Text (Text)

import Pipes
import qualified Pipes.Parse as PP

import Prelude hiding (take, takeWhile)

{-| Consume the first character from a 'Text' stream

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

{-| Draw one 'Char' from the underlying 'Producer', returning 'Nothing' if the
    'Producer' is empty
-}
drawChar :: (Monad m) => StateT (Producer Text m r) m (Maybe Char)
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
unDrawChar :: (Monad m) => Char -> StateT (Producer Text m r) m ()
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
peekChar :: (Monad m) => StateT (Producer Text m r) m (Maybe Char)
peekChar = do
    x <- drawChar
    case x of
        Nothing  -> return ()
        Just c -> unDrawChar c
    return x
{-# INLINABLE peekChar #-}

{-| Check if the underlying 'Producer' has no more characters

    Note that this will skip over empty 'Text' chunks, unlike
    'PP.isEndOfInput' from @pipes-parse@.

> isEndOfChars = liftM isLeft peekChar
-}
isEndOfChars :: (Monad m) => StateT (Producer Text m r) m Bool
isEndOfChars = do
    x <- peekChar
    return (case x of
        Nothing -> True
        Just _-> False )
{-# INLINABLE isEndOfChars #-}

{-| @(take n)@ only allows @n@ characters to pass

    Unlike 'take', this 'PP.unDraw's unused characters
-}
take :: (Monad m, Integral a) => a -> Pipe Text Text (StateT (Producer Text m r) m) ()
take n0 = go n0 where
    go n
        | n <= 0 = return ()
        | otherwise = do
            txt <- await
            let len = fromIntegral (T.length txt)
            if (len > n)
                then do
                    let n' = fromIntegral n
                    lift . PP.unDraw $ T.drop n' txt
                    yield $ T.take n' txt
                else do
                    yield txt
                    go (n - len)
{-# INLINABLE take #-}

{-| Take characters until they fail the predicate

    Unlike 'takeWhile', this 'PP.unDraw's unused characters
-}
takeWhile
    :: (Monad m)
    => (Char -> Bool)
    -> Pipe Text Text (StateT (Producer Text m r) m) ()
takeWhile predicate = go
  where
    go = do
        txt <- await
        let (prefix, suffix) = T.span predicate txt
        if (T.null suffix)
            then do
                yield txt
                go
            else do
                lift $ PP.unDraw suffix
                yield prefix
{-# INLINABLE takeWhile #-}
