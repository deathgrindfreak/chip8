{-# LANGUAGE OverloadedStrings #-}

module Main where

import Chip8
import Foreign.C.Types (CInt)
import SDL
import Linear (V4(..))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Vector.Storable as V
import Data.StateVar
import Data.IORef
import qualified Data.List as L
import Debug.Trace

pixelSize :: CInt
pixelSize = 15

getWindowSize :: (CInt, CInt) -> V2 CInt
getWindowSize (w, h) = V2 (w * pixelSize) (h * pixelSize)

scanCodes :: [Scancode]
scanCodes = [ Scancode1
            , Scancode2
            , Scancode3
            , Scancode4
            , ScancodeQ
            , ScancodeW
            , ScancodeE
            , ScancodeR
            , ScancodeA
            , ScancodeS
            , ScancodeD
            , ScancodeF
            , ScancodeZ
            , ScancodeX
            , ScancodeC
            , ScancodeV
            ]

chip8Window :: WindowConfig
chip8Window = WindowConfig
  { windowBorder = True
  , windowHighDPI = False
  , windowInputGrabbed = False
  , windowMode = Windowed
  , windowGraphicsContext = NoGraphicsContext
  , windowPosition = Wherever
  , windowResizable = False
  , windowInitialSize = getWindowSize displaySize
  , windowVisible = True
  }

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Chip 8" chip8Window
  renderer <- createRenderer window (-1) defaultRenderer
  -- program <- loadProgramFromFile "./roms/test_opcode.ch8"
  program <- loadProgramFromFile "./roms/ibm_logo.ch8"
  chip <- newIORef (initChip program)

  -- Clear the screen initially
  rendererDrawColor renderer $= V4 0 0 0 0
  clear renderer

  appLoop renderer chip
  destroyWindow window

appLoop :: Renderer -> IORef Chip -> IO ()
appLoop renderer chip = do
  events <- pollEvents
  let chipKeys = filterChipKeys events

  runChipCycle renderer chip chipKeys

  present renderer
  appLoop renderer chip

filterChipKeys :: [Event] -> [Int]
filterChipKeys = foldr findChipKey []
  where
    getScanCode event =
      case eventPayload event of
        KeyboardEvent keyboardEvent ->
          if keyboardEventKeyMotion keyboardEvent == Pressed
          then Just $ keysymScancode (keyboardEventKeysym keyboardEvent)
          else Nothing
        _ -> Nothing

    findChipKey event keys =
      maybe keys (:keys) (getScanCode event >>= (`L.elemIndex` scanCodes))

runChipCycle :: (MonadIO m) => Renderer -> IORef Chip -> [Int] -> m ()
runChipCycle renderer chip chipKeys = do
  chip $~ runCycle chipKeys

  Chip { display = ChipDisplay on off } <- get chip

  rendererDrawColor renderer $= V4 0 0 0 0
  drawPixels renderer off

  rendererDrawColor renderer $= V4 255 255 255 255
  drawPixels renderer on

drawPixels :: (MonadIO m) => Renderer -> V.Vector (V2 CInt) -> m ()
drawPixels renderer pixels = fillRects renderer $ V.map scalePixels pixels
  where scalePixels (V2 x y) = let x' = pixelSize * x
                                   y' = pixelSize * y
                                   startPoint = V2 x' y'
                                   endPoint = V2 pixelSize pixelSize
                               in Rectangle (P startPoint) endPoint

drawPixel :: (MonadIO m) => Renderer -> V2 CInt -> m ()
drawPixel renderer (V2 x y) =
  let x' = pixelSize * x
      y' = pixelSize * y
      startPoint = V2 x' y'
      endPoint = V2 pixelSize pixelSize
  in fillRect renderer $ Just $ Rectangle (P startPoint) endPoint
