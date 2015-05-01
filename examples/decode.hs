-- http://www.haskellforall.com/2014/02/pipes-parse-30-lens-based-parsing.html

import Data.ByteString (ByteString)
import Data.Text       (Text)
import Lens.Family.State.Strict (zoom)

import Pipes
import Pipes.Parse
import qualified Pipes.ByteString as ByteString
import qualified Pipes.Text as Text
import qualified Pipes.Text.Encoding as Text

-- Retrieve utf8-encode `Text` chunk(s) up to 10 characters
-- from the bytestring (this can have various byte lengths)
parser :: Monad m => Parser ByteString m [Text]
parser = zoom (Text.utf8 . Text.splitAt 10) drawAll

main = do
    (textChunks, leftovers) <- runStateT parser ByteString.stdin
    print textChunks

    -- Now print the remaining `ByteString` chunks
    byteChunks <- evalStateT drawAll leftovers
    print byteChunks
{-
$ ./decode
Hello, 世界!!!<Enter>
["Hello, \19990\30028!"]
abcdefg<Enter>
<Ctrl-D>
["!!\n","abcdefg\n"]

-}