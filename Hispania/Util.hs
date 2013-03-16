module Hispania.Util where

import Data.List
import Data.Word
import Data.Bits
import qualified Data.ByteString as BS
import System.Time


data Generator = Generator Word

symBase :: Word
symBase = 97

digBase :: Word
digBase = 48
  
nextVal :: Word -> ([Word], Word)
nextVal v = (symbols, nextSeed symbols)
  where
      z = v + 11
      mask r = let d = r .&. 31 in d `xor` (shift d 1)
      toDigits = unfoldr (\x -> if x > 0 then (Just ((mask x), (shiftR x 3))) else Nothing)
      toSymbols s = if s > 25 then (s - 25 + digBase) else (s + symBase)
      symbols = map toSymbols ((toDigits z) ++ (toDigits v))
      nextSeed x = sum (take ((fromIntegral ((head x) .&. 3)) + 1) x) + v

getSeed :: IO Word
getSeed = do
           (TOD seconds picoseconds) <- getClockTime
           return (fromIntegral seconds)

packThem :: [Word] -> BS.ByteString
packThem = BS.pack . (map fromIntegral)

liftM7 f ma mb mc md me mf mg =
  do
   ra <- ma
   rb <- mb
   rc <- mc
   rd <- md
   re <- me
   rf <- mf
   rg <- mg
   return (f ra rb rc rd re rf rg)