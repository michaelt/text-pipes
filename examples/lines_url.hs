{-# LANGUAGE OverloadedStrings #-}
-- https://gist.github.com/michaelt/88e1fac12876857deefe
-- following
-- https://gist.github.com/gelisam/c769d186493221d7ebbe and associated controversy.

module Main where
 
import Prelude hiding (lines)
import Lens.Family
import Pipes
import Pipes.Group
import Pipes.HTTP
import Pipes.Text
import Pipes.Text.Encoding
import Pipes.Text.IO (toHandle,stdout)
import qualified System.IO as IO
import Data.Functor (void)
import qualified Data.Text as T

main = do
  req <- parseUrl "https://gist.github.com/gelisam/c769d186493221d7ebbe"
                -- "http://www.example.com"
                -- "http://www.gutenberg.org/files/10/10-h/10-h.htm"
  withManager tlsManagerSettings $ \m ->
    withHTTP req m $ \resp ->  void $ runEffect $ 
         number_lines_of (responseBody resp ^. utf8 . lines) >-> toHandle IO.stdout

number_lines_of :: Monad m => FreeT (Producer Text m) m bad -> Producer Text m bad 
number_lines_of  = number_loop (1 :: Int) where
  number_loop n freeProducers = do
        freeProducer <- lift $ runFreeT freeProducers
        case freeProducer of
          Pure badbytes -> do yield $ T.pack "\n"
                              return badbytes -- these could be inspected ... 
          Free p -> do yield $ T.pack ("\n" ++ show n ++ "  ") 
                       nextFreeProducers <- p
                       number_loop (n+1) nextFreeProducers
