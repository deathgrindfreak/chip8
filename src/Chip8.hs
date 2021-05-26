module Chip8
  ( displaySize
  , initChip
  , Chip
  , Program

  , loadProgramFromFile

  , memory
  , index
  , decodeOp
  , displayOp
  )
where

import Prelude hiding (readFile)
import System.IO (FilePath)
import Data.ByteString.Lazy (readFile)
import Data.Word
import qualified Data.Vector as V
import Data.Bits
import qualified Data.Binary as B
import Data.Binary.Get (isEmpty, getWord8, runGet)

displaySize :: Num a => (a, a)
displaySize = (64, 32)

data Chip = Chip
  { memory :: V.Vector Int
  , display :: V.Vector Int
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
initChip program = loadProgram program . setFonts $ Chip
           { memory = V.replicate 4096 0
           , display = let (w, h) = displaySize in V.replicate (w * h) 0
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

data DecodedOp a = DecodedOp
  { nibbles :: [a]
  , secondByte :: a
  , trippleNibble :: a
  }
  deriving Show

decodeOp :: (Bits a, Num a) => a -> DecodedOp a
decodeOp op = DecodedOp
  { nibbles = [(op `shiftR` (4 * i)) .&. 0x0F | i <- [3,2..0]]
  , secondByte = op .&. 0xFF
  , trippleNibble = op .&. 0xFFF
  }

displayOp :: DecodedOp Int -> Chip -> Chip
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
                                  flattenedCoords =
                                    [flattenCoords (x + xi) (y + yi) | xi <- [0..7]
                                                                     , yi <- [0..(n-1)]
                                                                     , ((memory ch) V.! ((index ch) + yi)) `testBit` (7 - xi)
                                                                     , x + xi < w
                                                                     , y + yi < h]
                                  updatedPixelCoords = map determinePixel flattenedCoords
                              in ((display ch) V.// updatedPixelCoords, any ((== 0) . snd) updatedPixelCoords)

    -- Pixels are only set if current display pixel is empty.
    -- If a pixel is set where the pixel in the sprite is to be set, it is unset
    determinePixel pixelCoord = (pixelCoord, if (display ch) V.! pixelCoord == 0 then 1 else 0)

    -- Map 2D coords to a 1D vector index
    flattenCoords x y = w * y + x + (if y == 0 then 0 else 1)
