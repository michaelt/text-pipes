{-#LANGUAGE RankNTypes#-}


module Pipes.Text.IO 
   ( 
   -- * Simple line-based Text IO
   -- $lineio
   
   fromHandleLn
   , toHandleLn
   , stdinLn
   , stdoutLn
   , stdoutLn'
   , readFileLn
   , writeFileLn
  

   -- * Simple streaming text IO
   -- $textio
   
   -- * Caveats
   -- $caveats
   
   -- * Producers
   , fromHandle
   , stdin
   , readFile
   
   -- * Consumers
   , toHandle
   , stdout
   , writeFile
   
   -- * Re-exports
   , MonadSafe(..)
   , runSafeT
   , runSafeP
   , Safe.withFile
   ) where

import qualified System.IO as IO
import Control.Exception (throwIO, try)
import Foreign.C.Error (Errno(Errno), ePIPE)
import qualified GHC.IO.Exception as G
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Pipes
import qualified Pipes.Safe.Prelude as Safe
import Pipes.Safe (MonadSafe(..), runSafeT, runSafeP)
import Prelude hiding (readFile, writeFile)

{- $lineio
   Line-based operations are marked with a final \-@Ln@, like 'stdinLn', 'readFileLn'. They are
   drop-in replacements for the line-based operations in @Pipes.Prelude@ and
   @Pipes.Safe.Prelude@ - the final \-@Ln@ being added where necessary. 
   With them, one is producing, piping and consuming semantically significant individual texts, 
   understood as lines, just as one would pipe 'Int's. The standard materials from @Pipes@ and @Pipes.Prelude@ and
   @Data.Text@ are all you need to interact with these lines as you read or write them.
   You can use these operations without using any of the other material in this package. 

   Thus, to take a trivial case, here we upper-case three lines from standard input and write 
   them to a file.

>>> import Pipes
>>> import qualified Pipes.Prelude as P
>>> import qualified Pipes.Text.IO as Text
>>> import qualified Data.Text as T
>>> Text.runSafeT $ runEffect $ Text.stdinLn >-> P.take 3 >-> P.map T.toUpper >-> Text.writeFileLn "threelines.txt"
one<Enter>
two<Enter>
three<Enter>
>>> :! cat "threelines.txt"
ONE
TWO
THREE

   The point of view is very much that of @Pipes.Prelude@ and the user who needs no more
   can use them ignoring the rest of this package.

   The line-based operations are, however, subject to a number of caveats.
   First, where they read from a handle, they will of course happily 
   accumulate indefinitely long lines. This is likely to be legitimate for input 
   typed in by a user, and for locally produced log files and other known material, but
   otherwise not. See the post on
   <http://www.haskellforall.com/2013/09/perfect-streaming-using-pipes-bytestring.html perfect streaming> 
   to see why @pipes-bytestring@ and this package take a different approach. Furthermore, 
   like those in @Data.Text.IO@, the operations use the system encoding and @T.hGetLine@
   and thus are slower than the \'official\' route, which would use bytestring IO and
   the encoding and decoding functions in @Pipes.Text.Encoding@. Finally, they will generate
   text exceptions after the fashion of @Data.Text.Encoding@ rather than returning the 
   undigested bytes in the style of @Pipes.Text.Encoding@

-}


{-| Read separate lines of 'Text' from 'IO.stdin' using 'T.getLine' 
    This function will accumulate indefinitely long strict 'Text's. See the caveats above.

    Terminates on end of input
-}
stdinLn :: MonadIO m => Producer' T.Text m ()
stdinLn = fromHandleLn IO.stdin
{-# INLINABLE stdinLn #-}


{-| Write 'String's to 'IO.stdout' using 'putStrLn'

    Unlike 'toHandle', 'stdoutLn' gracefully terminates on a broken output pipe
-}
stdoutLn :: MonadIO m => Consumer' T.Text m ()
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

{-| Write lines of 'Text's to 'IO.stdout'.

    This does not handle a broken output pipe, but has a polymorphic return
    value.
-}
stdoutLn' :: MonadIO m => Consumer' T.Text m r
stdoutLn' = for cat (\str -> liftIO (T.putStrLn str))
{-# INLINABLE stdoutLn' #-}

{-# RULES
    "p >-> stdoutLn'" forall p .
        p >-> stdoutLn' = for p (\str -> liftIO (T.putStrLn str))
  #-}

{-| Read separate lines of 'Text' from a 'IO.Handle' using 'T.hGetLine'.
    This operation will accumulate indefinitely large strict texts. See the caveats above.

    Terminates on end of input
-}
fromHandleLn :: MonadIO m => IO.Handle -> Producer' Text m ()
fromHandleLn h =  go where
      getLine :: IO (Either G.IOException Text)
      getLine = try (T.hGetLine h)

      go = do txt <- liftIO getLine
              case txt of
                Left e  -> return ()
                Right y -> do yield y
                              go
{-# INLINABLE fromHandleLn #-}

-- to do: investigate differences from the above: 
-- fromHandleLn :: MonadIO m => IO.Handle -> Producer' T.Text m ()
-- fromHandleLn h = go
--   where
--     go = do
--         eof <- liftIO $ IO.hIsEOF h
--         unless eof $ do
--             str <- liftIO $ T.hGetLine h
--             yield str
--             go
-- {-# INLINABLE fromHandleLn #-}


-- | Write separate lines of 'Text' to a 'IO.Handle' using 'T.hPutStrLn'
toHandleLn :: MonadIO m => IO.Handle -> Consumer' T.Text m r
toHandleLn handle = for cat (\str -> liftIO (T.hPutStrLn handle str))
{-# INLINABLE toHandleLn #-}

{-# RULES
    "p >-> toHandleLn handle" forall p handle .
        p >-> toHandleLn handle = for p (\str -> liftIO (T.hPutStrLn handle str))
  #-}


{-| Stream separate lines of text from a file. This operation will accumulate
    indefinitely long strict text chunks. See the caveats above.
-}
readFileLn :: MonadSafe m => FilePath -> Producer Text m ()
readFileLn file = Safe.withFile file IO.ReadMode fromHandleLn
{-# INLINE readFileLn #-}



{-| Write lines to a file, automatically opening and closing the file as
    necessary
-}
writeFileLn :: (MonadSafe m) => FilePath -> Consumer' Text m r
writeFileLn file = Safe.withFile file IO.WriteMode toHandleLn
{-# INLINABLE writeFileLn #-}



{- $textio
    Where pipes @IO@ replaces lazy @IO@, @Producer Text IO r@ replaces lazy 'Text'. 
    The official IO of this package and the pipes ecosystem generally would use the
    IO functions in @Pipes.ByteString@ and the encoding and decoding material in 
    @Pipes.Text.Encoding@.

    The streaming functions exported here, namely, 'readFile', 'writeFile', 'fromHandle', 'toHandle', 
    'stdin' and 'stdout' simplify this and use the system encoding on the model of @Data.Text.IO@ 
    and @Data.Text.Lazy.IO@  Some caveats described below. 
    
    The main points are as in 
    <https://hackage.haskell.org/package/pipes-bytestring-1.0.0/docs/Pipes-ByteString.html Pipes.ByteString>:
    
    A 'Handle' can be associated with a 'Producer' or 'Consumer' according 
    as it is read or written to.
    
> import Pipes
> import qualified Pipes.Text as Text
> import qualified Pipes.Text.IO as Text
> import System.IO
>
> main =
>     withFile "inFile.txt"  ReadMode  $ \hIn  ->
>     withFile "outFile.txt" WriteMode $ \hOut ->
>     runEffect $ Text.fromHandle hIn >-> Text.toHandle hOut

To stream from files, the following is perhaps more Prelude-like (note that it uses Pipes.Safe):

> import Pipes
> import qualified Pipes.Text as Text
> import qualified Pipes.Text.IO as Text
> import Pipes.Safe
>
> main = runSafeT $ runEffect $ Text.readFile "inFile.txt" >-> Text.writeFile "outFile.txt"

    Finally, you can stream to and from 'stdin' and 'stdout' using the predefined 'stdin'
    and 'stdout' pipes, as with the following \"echo\" program:

> main = runEffect $ Text.stdin >-> Text.stdout

    These programs, unlike the corresponding programs written with the line-based functions,
    will pass along a 1 terabyte line without affecting memory use. 

-}


{- $caveats

    The operations exported here are a convenience, like the similar operations in 
    @Data.Text.IO@  (or rather, @Data.Text.Lazy.IO@, since, again, @Producer Text m r@ is
    'effectful text' and something like the pipes equivalent of lazy Text.)

    * Like the functions in @Data.Text.IO@, they attempt to work with the system encoding. 
  
    * Like the functions in @Data.Text.IO@, they significantly slower than ByteString operations. Where
       you know what encoding you are working with, use @Pipes.ByteString@ and @Pipes.Text.Encoding@ instead,
       e.g. @view utf8 Bytes.stdin@ instead of @Text.stdin@
  
    * Like the functions in  @Data.Text.IO@ , they use Text exceptions, not the standard Pipes protocols. 

-}

{-| Convert a 'IO.Handle' into a text stream using a text size 
    determined by the good sense of the text library. Note with the remarks 
    at the head of this module that this
    is  slower than @view utf8 (Pipes.ByteString.fromHandle h)@
    but uses the system encoding and has other nice @Data.Text.IO@ features
-}

fromHandle :: MonadIO m => IO.Handle -> Producer Text m ()
fromHandle h =  go where
      go = do txt <- liftIO (T.hGetChunk h)
              if T.null txt then return ()
                            else do yield txt
                                    go 
{-# INLINABLE fromHandle#-}


-- | Stream text from 'stdin'
stdin :: MonadIO m => Producer Text m ()
stdin = fromHandle IO.stdin
{-# INLINE stdin #-}


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



-- | Stream text into a file. Uses @pipes-safe@.
writeFile :: (MonadSafe m) => FilePath -> Consumer' Text m ()
writeFile file = Safe.withFile file IO.WriteMode toHandle
{-# INLINE writeFile #-}
