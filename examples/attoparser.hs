import Pipes
import Pipes.Text.IO (fromHandle)
import Pipes.Attoparsec (parsed)
import qualified System.IO as IO

data Test = Test {
  a :: Int,
  b :: Int
  } deriving (Show)

testParser :: Parser Test
testParser = do
  a <- decimal
  space
  b <- decimal
  endOfLine
  return $ Test a b
  
main = IO.withFile "./testfile" IO.ReadMode $ \handle -> runEffect $
    for test_parser (lift . print)
  where (parsed (testParser <* endOfLine) (fromHandle handle))