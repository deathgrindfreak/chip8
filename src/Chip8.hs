module Chip8
  ( displaySize
  , initChip
  , Chip(..)
  , Program
  , loadProgramFromFile
  , runCycle

  , decodeOp
  , displayOp
  , printDisplay
  )
where

import Prelude hiding (readFile)
import System.IO (FilePath)
import Data.ByteString.Lazy (readFile)
import Data.Word
import Linear (V2(..))
import Foreign.C.Types (CInt)
import qualified Data.Vector.Storable as V
import Data.Bits
import qualified Data.Binary as B
import Data.Binary.Get (isEmpty, getWord8, runGet)
import Data.List (partition, (\\), union, intersect)

import Numeric (showHex)
import Text.Printf
import Debug.Trace (trace)

displaySize :: Num a => (a, a)
displaySize = (64, 32)

data Chip = Chip
  { memory :: V.Vector Int
  , display :: V.Vector (V2 CInt)
  , pc :: Int
  , index :: Int
  , stack :: [Int]
  , delay :: Int
  , sound :: Int
  , registers :: V.Vector Int
  }
  deriving Show

type Program = [Int]

loadProgramFromFile :: FilePath -> IO Program
loadProgramFromFile program = do
  input <- readFile program
  return $ runGet readBytes input
  where
    readBytes = do
      e <- isEmpty
      if e
        then return []
        else do
          b <- getWord8
          bs <- readBytes
          return ((fromIntegral b):bs)

initChip :: Program -> Chip
initChip program = loadProgram program . setFonts $ ch
  where ch = Chip { memory = V.replicate 4096 0
                  , display = V.empty
                  , pc = 0x200
                  , index = 0
                  , stack = []
                  , delay = 0
                  , sound = 0
                  , registers = V.replicate 15 0
                  }

-- We'll assume programs start at 0x200
loadProgram :: Program -> Chip -> Chip
loadProgram program ch = ch { memory = (memory ch) V.// (zip [0x200..] program) }

-- Apparently it's cool to set the fonts at 0x50 to 0x9F, so I'll try to be cool too
setFonts :: Chip -> Chip
setFonts ch = ch { memory = (memory ch) V.// (zip [0x50..0x9F] fonts) }
  where fonts =
          [
            0xF0, 0x90, 0x90, 0x90, 0xF0, -- 0
            0x20, 0x60, 0x20, 0x20, 0x70, -- 1
            0xF0, 0x10, 0xF0, 0x80, 0xF0, -- 2
            0xF0, 0x10, 0xF0, 0x10, 0xF0, -- 3
            0x90, 0x90, 0xF0, 0x10, 0x10, -- 4
            0xF0, 0x80, 0xF0, 0x10, 0xF0, -- 5
            0xF0, 0x80, 0xF0, 0x90, 0xF0, -- 6
            0xF0, 0x10, 0x20, 0x40, 0x40, -- 7
            0xF0, 0x90, 0xF0, 0x90, 0xF0, -- 8
            0xF0, 0x90, 0xF0, 0x10, 0xF0, -- 9
            0xF0, 0x90, 0xF0, 0x90, 0x90, -- A
            0xE0, 0x90, 0xE0, 0x90, 0xE0, -- B
            0xF0, 0x80, 0x80, 0x80, 0xF0, -- C
            0xE0, 0x90, 0x90, 0x90, 0xE0, -- D
            0xF0, 0x80, 0xF0, 0x80, 0xF0, -- E
            0xF0, 0x80, 0xF0, 0x80, 0x80  -- F
          ]

runCycle :: Chip -> Chip
runCycle ch = let (ch', op) = fetch ch
              in execute (decodeOp op) ch'

fetch :: Chip -> (Chip, Int)
fetch ch = let pcv = (pc ch)
               firstByte = (memory ch) V.! pcv
               secondByte = (memory ch) V.! (pcv + 1)
               nextInstruction = firstByte * (16 ^ 2) + secondByte
           in (ch { pc = pcv + 2 }, nextInstruction)

data DecodedOp = DecodedOp
  { nibbles :: [Int]
  , secondByte :: Int
  , trippleNibble :: Int
  }
  deriving Show

decodeOp :: Int -> DecodedOp
decodeOp op = DecodedOp
  { nibbles = [(op `shiftR` (4 * i)) .&. 0x0F | i <- [3,2..0]]
  , secondByte = op .&. 0xFF
  , trippleNibble = op .&. 0xFFF
  }

type ExecuteOperation = DecodedOp -> Chip -> Chip

execute :: ExecuteOperation
execute op ch = let (fst:snd:thd:fth:_) = (nibbles op) in
  case (fst, snd, thd, fth) of
    (0x0, 0x0, 0xE, 0x0) -> clearScreen op ch
    (0x1, _, _, _) -> jump op ch
    (0x6, _, _, _) -> setRegister op ch
    (0x7, _, _, _) -> addToRegister op ch
    (0xA, _, _, _) -> setIndexRegister op ch
    (0xD, _, _, _) -> displayOp op ch

clearScreen :: ExecuteOperation
clearScreen op ch = ch { display = V.empty }

jump :: ExecuteOperation
jump op ch = ch { pc = (trippleNibble op) }

setRegister :: ExecuteOperation
setRegister op ch = ch { registers = (registers ch) V.// [((nibbles op) !! 1, secondByte op)] }

addToRegister :: ExecuteOperation
addToRegister op ch = let address = (nibbles op) !! 1
                          newValue = secondByte op + (registers ch) V.! address
                      in ch { registers = (registers ch) V.// [(address, newValue)] }

setIndexRegister :: ExecuteOperation
setIndexRegister op ch = ch { index = (trippleNibble op) }

displayOp :: ExecuteOperation
displayOp op ch = ch { display = updatedDisplay
                     , registers = if setVF
                                   then (registers ch) V.// [(0xF - 1, 1)]
                                   else (registers ch)
                     }
  where
    (w, h) = displaySize

    (updatedDisplay, setVF) = let (_:vx:vy:n:_) = nibbles op
                                  x = ((registers ch) V.! vx) `mod` w
                                  y = ((registers ch) V.! vy) `mod` h
                                  coords = [toCIntV2 (x + xi) (y + yi) | xi <- [0..7]
                                                                       , yi <- [0..(n-1)]
                                                                       , ((memory ch) V.! ((index ch) + yi)) `testBit` (7 - xi)
                                                                       , x + xi < w
                                                                       , y + yi < h
                                                                       ]

                                  -- We want the xor'd list of coordinates, or basically the union - intersection
                                  displayList = V.toList (display ch)
                                  intersection = coords `intersect` displayList
                                  xordPixels = V.fromList $ (coords `union` displayList) \\ intersection
                              in (xordPixels, length intersection > 0)

    toCIntV2 x y = V2 (fromIntegral x) (fromIntegral y)

printDisplay :: Chip -> IO ()
printDisplay ch = mapM_ putStrLn (chunk (fst displaySize) . toLines . display $ ch)
  where
    toLines display = V.foldr (\d a -> (if d == 0 then '.' else '#') : a) [] display

    chunk :: Int -> [a] -> [[a]]
    chunk _ [] = []
    chunk n xs = (take n xs) : (chunk n (drop n xs))
