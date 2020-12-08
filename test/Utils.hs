{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import Control.Exception (SomeException, bracket, bracket_, evaluate, try)
import Data.Bits ((.&.))
import qualified Data.ByteString as B
import Data.Char (chr)
import Data.String (IsString, fromString)
import Debug.Trace (trace)
import Pipes.Text.Internal
import System.IO.Unsafe (unsafePerformIO)
import System.Random (Random (..), RandomGen)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Monadic (assert, monadicIO, run)

-- Ensure that two potentially bottom values (in the sense of crashing
-- for some inputs, not looping infinitely) either both crash, or both
-- give comparable results for some input.
(=^=) :: (Eq a, Show a) => a -> a -> Bool
i =^= j = unsafePerformIO $ do
  x <- try (evaluate i)
  y <- try (evaluate j)
  case (x, y) of
    (Left (_ :: SomeException), Left (_ :: SomeException)) ->
      return True
    (Right a, Right b) -> return (a == b)
    e -> trace ("*** Divergence: " ++ show e) return False

infix 4 =^=

{-# NOINLINE (=^=) #-}

-- Do two functions give the same answer?
eq :: (Eq a, Show a) => (t -> a) -> (t -> a) -> t -> Bool
eq a b s = a s =^= b s

-- What about with the RHS packed?
-- eqP :: (Eq a, Show a, Stringy s) =>
--        (String -> a) -> (s -> a) -> String -> Word8 -> Bool
-- eqP f g s w  = eql "orig" (f s) (g t) &&
--                eql "mini" (f s) (g mini) &&
--                eql "head" (f sa) (g ta) &&
--                eql "tail" (f sb) (g tb)
--     where t             = packS s
--           mini          = packSChunkSize 10 s
--           (sa,sb)       = splitAt m s
--           (ta,tb)       = splitAtS m t
--           l             = length s
--           m | l == 0    = n
--             | otherwise = n `mod` l
--           n             = fromIntegral w
--           eql d a b
--             | a =^= b   = True
--             | otherwise = trace (d ++ ": " ++ show a ++ " /= " ++ show b) False

instance Arbitrary B.ByteString where
  arbitrary = B.pack `fmap` arbitrary

genUnicode :: IsString a => Gen a
genUnicode = fmap fromString string
  where
    string = sized $ \n ->
      do
        k <- choose (0, n)
        sequence [char | _ <- [1 .. k]]

    excluding :: [a -> Bool] -> Gen a -> Gen a
    excluding bad gen = loop
      where
        loop = do
          x <- gen
          if or (map ($ x) bad)
            then loop
            else return x

    reserved = [lowSurrogate, highSurrogate, noncharacter]
    lowSurrogate c = c >= 0xDC00 && c <= 0xDFFF
    highSurrogate c = c >= 0xD800 && c <= 0xDBFF
    noncharacter c = masked == 0xFFFE || masked == 0xFFFF
      where
        masked = c .&. 0xFFFF

    ascii = choose (0, 0x7F)
    plane0 = choose (0xF0, 0xFFFF)
    plane1 =
      oneof
        [ choose (0x10000, 0x10FFF),
          choose (0x11000, 0x11FFF),
          choose (0x12000, 0x12FFF),
          choose (0x13000, 0x13FFF),
          choose (0x1D000, 0x1DFFF),
          choose (0x1F000, 0x1FFFF)
        ]
    plane2 =
      oneof
        [ choose (0x20000, 0x20FFF),
          choose (0x21000, 0x21FFF),
          choose (0x22000, 0x22FFF),
          choose (0x23000, 0x23FFF),
          choose (0x24000, 0x24FFF),
          choose (0x25000, 0x25FFF),
          choose (0x26000, 0x26FFF),
          choose (0x27000, 0x27FFF),
          choose (0x28000, 0x28FFF),
          choose (0x29000, 0x29FFF),
          choose (0x2A000, 0x2AFFF),
          choose (0x2B000, 0x2BFFF),
          choose (0x2F000, 0x2FFFF)
        ]
    plane14 = choose (0xE0000, 0xE0FFF)
    planes = [ascii, plane0, plane1, plane2, plane14]

    char = chr `fmap` excluding reserved (oneof planes)
