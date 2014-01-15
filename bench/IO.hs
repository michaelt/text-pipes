import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy as TL

import Pipes
import qualified Pipes.Text as TP
import qualified Pipes.ByteString as BP
import Pipes.Safe

main = textaction
big = "../../examples/txt/words2.txt"

textaction = T.readFile big >>= T.putStrLn
pipeaction =  runEffect $ for ((TP.readFile big) >> return ()) (lift . T.putStrLn)





