{-# LANGUAGE OverloadedStrings #-}

-- https://gist.github.com/michaelt/88e1fac12876857deefe
-- following
-- https://gist.github.com/gelisam/c769d186493221d7ebbe and associated controversy.

module Main where

import Data.Functor (void)
import qualified Data.Text as T
import Lens.Family
import Pipes
import Pipes.Group
import Pipes.HTTP
import Pipes.Text
import Pipes.Text.Encoding
import Pipes.Text.IO (stdout, toHandle)
import qualified System.IO as IO
import Prelude hiding (lines)

main = do
  req <- parseUrl "https://gist.github.com/gelisam/c769d186493221d7ebbe"
  -- "http://www.example.com"
  -- "http://www.gutenberg.org/files/10/10-h/10-h.htm"
  withManager tlsManagerSettings $ \m ->
    withHTTP req m $ \resp ->
      void $
        runEffect $
          number_lines_of (responseBody resp ^. utf8 . lines) >-> toHandle IO.stdout

number_lines_of :: Monad m => FreeT (Producer Text m) m r -> Producer Text m r
number_lines_of = number_loop (1 :: Int)
  where
    number_loop n free = do
      fproducer <- lift $ runFreeT free
      case fproducer of
        Pure badbytes -> do
          yield $ T.pack "\n"
          return badbytes -- these could be inspected ...
        Free p -> do
          yield $ T.pack ("\n" ++ show n ++ "  ")
          next_free <- p
          number_loop (n + 1) next_free
