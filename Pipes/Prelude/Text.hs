{-# LANGUAGE RankNTypes #-}

module Pipes.Prelude.Text
  ( -- * Simple line-based Text IO
    -- $lineio
    fromHandleLn,
    toHandleLn,
    stdinLn,
    stdoutLn,
    stdoutLn',
    readFileLn,
    writeFileLn,
  )
where

import Control.Exception (throwIO, try)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Foreign.C.Error (Errno (Errno), ePIPE)
import qualified GHC.IO.Exception as G
import Pipes
import Pipes.Safe (MonadSafe (..))
import qualified Pipes.Safe.Prelude as Safe
import qualified System.IO as IO
import Prelude hiding (readFile, writeFile)

-- $lineio
--   Line-based operations are marked with a final \-@Ln@, like 'stdinLn', 'readFileLn', etc.
--   They are drop-in 'Text' replacements for the corresponding 'String' operations in
--   @Pipes.Prelude@ and @Pipes.Safe.Prelude@ - a final \-@Ln@ being added where necessary.
--   This module can thus be imported unqualified if @Pipes.Prelude@ is imported qualified, as
--   it must be.
--
--   In using the line-based operations, one is producing and consuming semantically significant individual texts,
--   understood as lines, just as one would produce or pipe 'Int's or 'Char's or anything else.
--   The standard materials from @Pipes@ and @Pipes.Prelude@ and
--   @Data.Text@ are all you need to work with them, and
--   you can use these operations without using any of the other modules in this package.
--
--   Thus, to take a trivial case, here we upper-case three lines from standard input and write
--   them to a file.  (@runSafeT@ from @Pipes.Safe@ just makes sure to close any handles opened in its scope;
--   it is only needed for @readFileLn@ and @writeFileLn@.)
--
-- >>> import Pipes
-- >>> import qualified Pipes.Prelude as P
-- >>> import qualified Pipes.Prelude.Text as Text
-- >>> import qualified Data.Text as T
-- >>> Text.runSafeT $ runEffect $ Text.stdinLn >-> P.take 3 >-> P.map T.toUpper >-> Text.writeFileLn "threelines.txt"
-- one<Enter>
-- two<Enter>
-- three<Enter>
-- >>> :! cat "threelines.txt"
-- ONE
-- TWO
-- THREE
--
--   The point of view is very much that of @Pipes.Prelude@, substituting @Text@ for @String@.
--   It would still be the same even if
--   we did something a bit more sophisticated, like run an ordinary attoparsec 'Text' parser on
--   each line, as is frequently desirable.  Here we use
--   a minimal attoparsec number parser, @scientific@, on separate lines of standard input,
--   dropping bad parses with @P.concat@:
--
-- >>> import Data.Attoparsec.Text (parseOnly, scientific)
-- >>> P.toListM $ Text.stdinLn >-> P.takeWhile (/= "quit") >-> P.map (parseOnly scientific) >-> P.concat
-- 1<Enter>
-- 2<Enter>
-- bad<Enter>
-- 3<Enter>
-- quit<Enter>
-- [1.0,2.0,3.0]
--
--   The line-based operations are, however, subject to a number of caveats.
--
--   * Where these line-based operations read from a handle, they will
--     accumulate indefinitely long lines. This makes sense for input
--     typed in by a user, and for locally produced files of known characteristics, but
--     otherwise not. See the post on
--     <http://www.haskellforall.com/2013/09/perfect-streaming-using-pipes-bytestring.html perfect streaming>
--     to see why @pipes-bytestring@ and this package, outside this module, take a different approach, in which
--     lines themselves are permitted to stream without accumulation.
--
--   * The line-based operations,
--     like those in @Data.Text.IO@, use the system encoding (and @T.hGetLine@, @T.hPutLine@ etc.)
--     and thus are slower than the \'official\' route, which would use the very fast
--     bytestring IO operations from @Pipes.ByteString@ and the
--     encoding and decoding functions in @Pipes.Text.Encoding@, which are also quite fast
--     thanks to the @streaming-commons@ package.
--
--   * The line-based operations (again like those in @Data.Text.IO@) will
--     generate text exceptions after the fashion of
--     @Data.Text.Encoding@, rather than returning the undigested bytes in the
--     style of @Pipes.Text.Encoding@. This is the standard practice in the pipes libraries.

-- | Read separate lines of 'Text' from 'IO.stdin' using 'T.getLine', terminating on end of input.
--
--    This function will accumulate indefinitely long strict 'Text's. See the caveats above.
stdinLn :: MonadIO m => Producer' T.Text m ()
stdinLn = fromHandleLn IO.stdin
{-# INLINEABLE stdinLn #-}

-- | Write 'Text' lines to 'IO.stdout' using 'putStrLn', terminating without error on a broken output pipe
stdoutLn :: MonadIO m => Consumer' T.Text m ()
stdoutLn = go
  where
    go = do
      str <- await
      x <- liftIO $ try (T.putStrLn str)
      case x of
        Left
          G.IOError
            { G.ioe_type = G.ResourceVanished,
              G.ioe_errno = Just ioe
            }
            | Errno ioe == ePIPE ->
              return ()
        Left e -> liftIO (throwIO e)
        Right () -> go
{-# INLINEABLE stdoutLn #-}

-- | Write lines of 'Text' to 'IO.stdout'. This does not handle a broken output pipe,
--    but has a polymorphic return value.
stdoutLn' :: MonadIO m => Consumer' T.Text m r
stdoutLn' = for cat (liftIO . T.putStrLn)
{-# INLINE [1] stdoutLn' #-}

{-# RULES
"p >-> stdoutLn'" forall p.
  p >-> stdoutLn' =
    for p (liftIO . T.putStrLn)
  #-}

-- | Read separate lines of 'Text' from a 'IO.Handle' using 'T.hGetLine',
--    terminating at the end of input
--
--    This operation will accumulate indefinitely large strict texts. See the caveats above.
fromHandleLn :: MonadIO m => IO.Handle -> Producer' Text m ()
fromHandleLn h = go
  where
    getLine :: IO (Either G.IOException Text)
    getLine = try (T.hGetLine h)

    go = do
      txt <- liftIO getLine
      case txt of
        Left _ -> return ()
        Right y -> do
          yield y
          go
{-# INLINEABLE fromHandleLn #-}

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
toHandleLn handle = for cat (liftIO . T.hPutStrLn handle)
{-# INLINE [1] toHandleLn #-}

{-# RULES
"p >-> toHandleLn handle" forall p handle.
  p >-> toHandleLn handle =
    for p (liftIO . T.hPutStrLn handle)
  #-}

-- | Stream separate lines of text from a file. Apply @runSafeT@ after running the
--    pipeline to manage the opening and closing of the handle.
--
--    This operation will accumulate indefinitely long strict text chunks.
--    See the caveats above.
readFileLn :: MonadSafe m => FilePath -> Producer Text m ()
readFileLn file = Safe.withFile file IO.ReadMode (\h -> fromHandleLn h)
{-# INLINE readFileLn #-}

-- | Write lines to a file. Apply @runSafeT@ after running the
--    pipeline to manage the opening and closing of the handle.
writeFileLn :: (MonadSafe m) => FilePath -> Consumer' Text m r
writeFileLn file = Safe.withFile file IO.WriteMode (\h -> toHandleLn h)
{-# INLINEABLE writeFileLn #-}
