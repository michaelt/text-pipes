import Pipes
import Pipes.Text.IO (fromHandle)
import Pipes.Attoparsec (parsed)
import qualified System.IO as IO
import Data.Attoparsec.Text
import Control.Applicative
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
   do leftover <- for (parsed testParser (fromHandle handle)) 
                   (lift . print)
      return () -- ignore unparsed material
      
-- >>> :! cat testfile
-- 1 1
-- 2 2
-- 3 3
-- 4 4
-- 5 5
-- 6 6
-- 7 7
-- 8 8
-- 9 9
-- 10 10

-- >>> main
-- Test {a = 1, b = 1}
-- Test {a = 2, b = 2}
-- Test {a = 3, b = 3}
-- Test {a = 4, b = 4}
-- Test {a = 5, b = 5}
-- Test {a = 6, b = 6}
-- Test {a = 7, b = 7}
-- Test {a = 8, b = 8}
-- Test {a = 9, b = 9}
-- Test {a = 10, b = 10}


