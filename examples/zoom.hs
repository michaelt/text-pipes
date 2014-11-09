-- this file illustrates several uses of `zoom` 
-- one of them is quadratic in the length of the file
-- since it has to decode and encode repeatedly,
-- and is thus no good on long files. 

{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE BangPatterns#-}
{-# LANGUAGE RankNTypes #-}
import           Blaze.ByteString.Builder  (Builder, fromByteString, toByteString)
import           Control.Exception         (Exception)
import           Control.Monad.Trans.Class (lift)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as S
import qualified Data.ByteString.Lazy      as L
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TEE
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TLE

import Pipes
import Pipes.Parse
import qualified Pipes.Prelude as PP
import qualified Pipes.ByteString as Bytes
import qualified Pipes.Text as Txt
import Pipes.Text.Encoding (utf8)

import Control.Lens
import Control.Lens.Internal.Zoom
import Control.Monad
import qualified System.IO as IO
import Control.Monad.Trans.Maybe
import Control.Monad.State.Class

main :: IO ()
main = do -- S.writeFile fp $ contents 10000 -- 10000 cannot be handled fileParser0 and 1
          -- parse_file fileParser0  -- pathological
          -- parse_file fileParser1  -- programs
          parse_file fileParser2  -- good program 
          
   where 
   parse_file parser = IO.withBinaryFile fp IO.ReadMode $ \h ->
                         do p' <- runEffect $ parseWith parser ( Bytes.fromHandle h ) >-> PP.print
                            runEffect $ p' >-> PP.print
   parseWith parser = loop where
      loop p = do (m,p') <- lift (runStateT (runMaybeT parser) p)
                  case m of Nothing -> return p'
                            Just file -> do yield file
                                            loop p'
   fp = "encoded.fileformat"
   contents n =  (toByteString . mconcat . replicate n . encodeFiles) input
                 <> S.pack (replicate 10 250)



fileParser0, fileParser1, fileParser2 :: Monad m => MaybeT (StateT (Producer ByteString m x) m) File
fileParser0  = do (name, len) <- zoom utf8 parseText
                  contents    <- zoom (Bytes.splitAt len) (lift drawAll)
                  return (File name (S.concat contents))
    where
    -- this parser aggregates all Text parsing into one preliminary parser
    -- which is then applied with `zoom utf8`
    -- we cannot tell in advance how long, e.g. the file name will be
    parseText :: Monad m => MaybeT (StateT (Producer Text m x) m) (Text, Int)
    parseText = do nameLength    <- parseNumber
                   names         <- zoom (Txt.splitAt nameLength) $ (lift drawAll)
                   contentLength <- parseNumber
                   return $! (T.concat names, contentLength)

-- here we disaggregate the little Text parsers but still apply them with `zoom utf8`
-- this makes no difference
fileParser1  = do nameLength    <- zoom utf8 parseNumber
                  names         <- zoom (utf8 . Txt.splitAt nameLength)  (lift drawAll)
                  contentLength <- zoom utf8 parseNumber
                  contents      <- zoom (Bytes.splitAt contentLength) (lift drawAll)
                  return (File (T.concat names) (S.concat contents))

-- this is the good program; be reflecting on the fact that file names
-- should not be a 1000 bytes long, and binary files longer than e.g. 10 ^ 10
-- we can restrict the length of the byte stream to which we apply `zoom utf8`
fileParser2  = do nameLength  <- Bytes.splitAt 3 ~~> utf8 ~~> parseNumber
                  names       <- Bytes.splitAt nameLength ~~> utf8 ~~> lift drawAll
                  len         <- Bytes.splitAt 10 ~~>  utf8 ~~> parseNumber
                  contents    <- Bytes.splitAt len ~~> lift drawAll
                  return (File (T.concat names) (S.concat contents))

-- infix lens nonsense
infixr 1 ~~>
(~~>) :: Zoom m n s t 
      => ((s -> Zoomed n c s) -> t -> Zoomed n c t)
      -> m c -> n c
(~~>) = zoom
{-# INLINE (~~>) #-}

parseNumber :: Monad m =>  MaybeT (StateT (Producer Text m x) m) Int
parseNumber  = loop  0 where
   loop !n = do c <- MaybeT  Txt.drawChar
                case c of ':' -> return n
                          _   -> do guard ('0' <= c && c <= '9')
                                    loop  $! n * 10 + (fromEnum c - fromEnum '0')



-- --- Michael S's `File` type and its binary encoding, etc.


data File = File
    { fileName     :: !Text
    , fileContents :: !ByteString
    }
    deriving Show

encodeFile :: File -> Builder
encodeFile (File name contents) =
    tellLength (S.length bytesname) <>
    fromByteString bytesname        <>
    tellLength (S.length contents)  <>
    fromByteString contents
  where
    tellLength i = fromByteString $ TEE.encodeUtf8 (T.pack (shows i ":"))
    bytesname = TEE.encodeUtf8 name

encodeFiles :: [File] -> Builder
encodeFiles = mconcat . map encodeFile

input :: [File]
input =
    [ File "utf8.txt" $ TEE.encodeUtf8 "This file is in UTF-8"
    , File "utf16.txt" $ TEE.encodeUtf16LE "This file is in UTF-16"
    , File "binary.dat" "we'll pretend to be binary"
    ]


---

-- This desperate scheme actually has some efficacy, if used before `utf8` in a zoom
-- but not much 

chunk :: Monad m => Int -> Lens' (Producer ByteString m r) (Producer ByteString m r)
chunk n = lens (chunkyN n) (\_ b -> b) where

    chunkyN  :: Monad m => Int -> Producer ByteString m r -> Producer ByteString m r
    chunkyN n  = prod_loop where

      prod_loop p = do mbs <- lift $ next p
                       case mbs of Left r -> return r
                                   Right (bs, p') -> do bs_loop bs
                                                        prod_loop p'
      bs_loop bs = unless (S.null bs) $ do yield fore
                                           unless (S.null aft)  (bs_loop aft)
            where (fore, aft) = S.splitAt n bs
