{-#LANGUAGE RankNTypes#-}

module Pipes.Text.IO 
   ( stdin
   , stdout
   , fromHandle
   , toHandle
   , readFile
   , writeFile
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
import qualified Pipes.Safe as Safe
import Pipes.Safe (MonadSafe(..), Base(..))
import Prelude hiding (readFile, writeFile)

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
              if T.null txt then return ()
                            else do yield txt
                                    go 
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
