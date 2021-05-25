module Chip8
  ( displaySize
  , initChip
  , Chip
  , Program
  )
where

import Data.Word
import qualified Data.Vector as V
import Data.Bits

displaySize :: Num a => (a, a)
displaySize = (64, 32)

data Chip = Chip
  { memory :: V.Vector Word8
  , display :: V.Vector Word8
  , pc :: Word8
  , index :: Word16
  , stack :: [Word16]
  , delay :: Word8
  , sound :: Word8
  , registers :: V.Vector Word8
  }
  deriving Show

type Program = [Word8]

initChip :: Program -> Chip
initChip program = loadProgram program . setFonts $ Chip
           { memory = V.replicate 4096 0
           , display = let (w, h) = displaySize in V.replicate (w * h) 0
           , pc = 0
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

decodeOp :: (Bits a, Num a) => a -> DecodedOp a
decodeOp op = DecodedOp
  { nibbles = [(op `shiftR` (4 * i)) .&. 0x0F | i <- [3,2..0]]
  , secondByte = op .&. 0xFF
  , trippleNibble = op .&. 0xFFF
  }
