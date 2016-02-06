{-#LANGUAGE RankNTypes#-}


module Pipes.Prelude.Text
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
   like those in @Data.Text.IO@, the operations use the system encoding (and @T.hGetLine@)
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

