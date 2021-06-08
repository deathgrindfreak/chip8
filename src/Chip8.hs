module Chip8
  ( displaySize
  , initChip
  , Chip(..)
  , ChipDisplay(..)
  , ChipExtData(..)
  , Program
  , loadProgramFromFile
  , runCycle

  , decodeOp
  , displayOp
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
import System.Random (StdGen, randomR)

import Numeric (showHex)
import Text.Printf
import Debug.Trace (trace)

displaySize :: Num a => (a, a)
displaySize = (64, 32)

data ChipDisplay = ChipDisplay (V.Vector (V2 CInt)) (V.Vector (V2 CInt))
  deriving Show

data Chip = Chip
  { memory :: V.Vector Int
  , display :: ChipDisplay
  , pc :: Int
  , index :: Int
  , stack :: [Int]
  , delay :: Int
  , sound :: Int
  , registers :: V.Vector Int
  , gen :: StdGen
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
          return (fromIntegral b:bs)

initChip :: Program -> StdGen -> Chip
initChip program g = loadProgram program . setFonts $ ch
  where ch = Chip { memory = V.replicate 4096 0
                  , display = ChipDisplay V.empty V.empty
                  , pc = 0x200
                  , index = 0
                  , stack = []
                  , delay = 0
                  , sound = 0
                  , registers = V.replicate 16 0
                  , gen = g
                  }

-- We'll assume programs start at 0x200
loadProgram :: Program -> Chip -> Chip
loadProgram program ch = ch { memory = memory ch V.// zip [0x200..] program }

-- Apparently it's cool to set the fonts at 0x50 to 0x9F, so I'll try to be cool too
setFonts :: Chip -> Chip
setFonts ch = ch { memory = memory ch V.// zip [0x50..0x9F] fonts }
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

data ChipExtData = ChipExtData
  { keys :: [Int]
  , millis :: Int
  }

runCycle :: ChipExtData -> Chip -> Chip
runCycle ext ch =
  let (ch', op) = fetch ext ch
  in execute ext (decodeOp op) ch'

fetch :: ChipExtData -> Chip -> (Chip, Int)
fetch extData ch =
  let byte1 = memory ch V.! pc ch
      byte2 = memory ch V.! (pc ch + 1)
      delay' = updateDelayTimer extData ch
      sound' = updateSoundTimer extData ch
  in (ch { pc = pc ch + 2, delay = delay', sound = sound' }, byte1 * (16 ^ 2) + byte2)

updateDelayTimer :: ChipExtData -> Chip -> Int
updateDelayTimer ChipExtData { millis = ms } Chip { delay = dl }
  | dl - d > 0 = dl - d
  | otherwise = 0
  where d = (ms - dl) * 60 `div` 1000

updateSoundTimer :: ChipExtData -> Chip -> Int
updateSoundTimer ChipExtData { millis = ms } Chip { sound = sd }
  | sd - d > 0 = sd - d
  | otherwise = 0
  where d = (ms - sd) * 60 `div` 1000

data DecodedOp = DecodedOp
  { nibbles :: (Int, Int, Int, Int)
  , secondByte :: Int
  , trippleNibble :: Int
  }
  deriving Show

decodeOp :: Int -> DecodedOp
decodeOp op = DecodedOp
  { nibbles = let (f:s:t:r:_) = [(op `shiftR` (4 * i)) .&. 0x0F | i <- [3,2..0]]
              in (f, s, t, r)
  , secondByte = op .&. 0xFF
  , trippleNibble = op .&. 0xFFF
  }

secondNibble :: DecodedOp -> Int
secondNibble DecodedOp { nibbles = (_, t, _, _) } = t

thirdNibble :: DecodedOp -> Int
thirdNibble DecodedOp { nibbles = (_, _, t, _) } = t

type ExecuteOperation = DecodedOp -> Chip -> Chip

execute :: ChipExtData -> ExecuteOperation
execute ChipExtData { keys = ks } op ch =
  case nibbles op of
    (0x0, 0x0, 0xE, 0x0) -> ch { display = ChipDisplay V.empty V.empty }
    (0x0, 0x0, 0xE, 0xE) -> let (returnPC:rsStack) = stack ch
                            in ch { stack = rsStack, pc = returnPC}
    (0x0, _, _, _) -> ch -- ignore
    (0x1, _, _, _) -> ch { pc = trippleNibble op }
    (0x2, _, _, _) -> ch { pc = trippleNibble op
                         , stack = pc ch : stack ch
                         }
    (0x3, _, _, _) -> conditionalSkip op ch
    (0x4, _, _, _) -> negConditionalSkip op ch
    (0x5, _, _, 0x0) -> registerSkip op ch
    (0x6, _, _, _) -> ch { registers = registers ch V.// [(secondNibble op, secondByte op)] }
    (0x7, _, _, _) -> addToRegister op ch
    (0x8, _, _, 0x0) -> registerOp Nothing (const id) op ch
    (0x8, _, _, 0x1) -> registerOp Nothing (.|.) op ch
    (0x8, _, _, 0x2) -> registerOp Nothing (.&.) op ch
    (0x8, _, _, 0x3) -> registerOp Nothing xor op ch
    (0x8, _, _, 0x4) -> registerOp (Just $ \r _ _ -> r > 255) (+) op ch
    (0x8, _, _, 0x5) -> registerOp (Just $ \r _ _ -> r > 0) (-) op ch
    (0x8, _, _, 0x6) -> registerOp (Just $ \_ x _ -> x `testBit` 0) (\vx _ -> vx `shiftR` 1) op ch
    (0x8, _, _, 0x7) -> registerOp (Just $ \r _ _ -> r > 0) (flip (-)) op ch
    (0x8, _, _, 0xE) -> registerOp (Just $ \_ x _ -> x `testBit` 7) (\vx _ -> vx `shiftL` 1) op ch
    (0x9, _, _, 0x0) -> negRegisterSkip op ch
    (0xA, _, _, _) -> ch { index = trippleNibble op }
    (0xB, _, _, _) -> ch { pc = trippleNibble op + (registers ch V.! 0) }
    (0xC, _, _, _) -> let (n, g') = randomR (0, 255) (gen ch)
                      in ch { gen = g'
                            , registers = registers ch V.// [(secondNibble op, secondByte op .&. n)]}
    (0xD, _, _, _) -> displayOp op ch
    (0xE, _, 0x9, 0xE) -> ch { pc = pc ch + if secondByte op `elem` ks then 2 else 0 }
    (0xE, _, 0xA, 0x1) -> ch { pc = pc ch + if secondByte op `notElem` ks then 2 else 0 }
    (0xF, _, 0x0, 0x7) -> ch { registers = registers ch V.// [(secondNibble op, delay ch)] }
    (0xF, _, 0x0, 0xA) -> waitForKeyPress ks op ch
    (0xF, _, 0x1, 0x5) -> ch { delay = registers ch V.! secondNibble op }
    (0xF, _, 0x1, 0x8) -> ch { sound = registers ch V.! secondNibble op }
    (0xF, _, 0x1, 0xE) -> ch { index = index ch + registers ch V.! secondNibble op }
    (0xF, _, 0x2, 0x9) -> ch -- TODO
    (0xF, _, 0x3, 0x3) -> ch -- TODO
    (0xF, _, 0x5, 0x5) -> ch { memory = memory ch V.// map (\v -> (index ch + v, registers ch V.! v)) [0x0..(secondNibble op)]}
    (0xF, _, 0x6, 0x5) -> ch { registers = registers ch V.// map (\v -> (v, memory ch V.! index ch + v)) [0x0..(secondNibble op)]}

skipOp :: (Chip -> Bool) -> Chip -> Chip
skipOp cond ch = if cond ch then ch { pc = pc ch + 2 } else ch

conditionalSkip :: ExecuteOperation
conditionalSkip op = skipOp $ \ch -> (registers ch V.! secondNibble op) == secondByte op

negConditionalSkip :: ExecuteOperation
negConditionalSkip op = skipOp $ \ch -> (registers ch V.! secondNibble op) /= secondByte op

registerSkip :: ExecuteOperation
registerSkip op = skipOp $ \ch ->
  (registers ch V.! secondNibble op) == (registers ch V.! thirdNibble op)

negRegisterSkip :: ExecuteOperation
negRegisterSkip op = skipOp $ \ch->
  (registers ch V.! secondNibble op) /= (registers ch V.! thirdNibble op)

addToRegister :: ExecuteOperation
addToRegister op ch = let address = secondNibble op
                          newValue = secondByte op + registers ch V.! address
                      in ch { registers = registers ch V.// [(address, newValue `mod` 256)] }

registerOp :: Maybe (Int -> Int -> Int -> Bool) -> (Int -> Int -> Int) -> ExecuteOperation
registerOp overflow f op ch =
  let vx = registers ch V.! secondNibble op
      vy = registers ch V.! thirdNibble op
      result = f vx vy
      registerValues =
        case overflow of
          Just setOverflow -> [(secondNibble op, result `mod` 256),
                                (0xF, if setOverflow result vx vy then 1 else 0)]
          Nothing -> [(secondNibble op, result)]
  in ch { registers = registers ch V.// registerValues }

displayOp :: ExecuteOperation
displayOp op ch = ch { display = updatedDisplay
                     , registers = if setVF
                                   then registers ch V.// [(0xF, 1)]
                                   else registers ch
                     }
  where
    (w, h) = displaySize

    (updatedDisplay, setVF) = let (_ , vx, vy, n) = nibbles op
                                  x = (registers ch V.! vx) `mod` w
                                  y = (registers ch V.! vy) `mod` h
                                  coords = [toCIntV2 (x + xi) (y + yi) | xi <- [0..7]
                                                                       , yi <- [0..(n-1)]
                                                                       , (memory ch V.! (index ch + yi)) `testBit` (7 - xi)
                                                                       , x + xi < w
                                                                       , y + yi < h
                                                                       ]

                                  -- We want the xor'd list of coordinates, or basically the union - intersection
                                  ChipDisplay onPixels _ = display ch
                                  displayList = V.toList onPixels
                                  intersection = coords `intersect` displayList
                                  xordPixels = V.fromList $ (coords `union` displayList) \\ intersection
                              in (ChipDisplay xordPixels (V.fromList intersection), null intersection)

    toCIntV2 x y = V2 (fromIntegral x) (fromIntegral y)

waitForKeyPress :: [Int] -> ExecuteOperation
waitForKeyPress ks op ch =
  if null ks
  then ch { pc = pc ch - 2 } -- Reset PC so that we loop at the same instruction
  else ch { registers = registers ch V.// [(secondByte op, head ks)] }
