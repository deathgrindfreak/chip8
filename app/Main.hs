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

pixelSize :: CInt
pixelSize = 15

getWindowSize :: (CInt, CInt) -> V2 CInt
getWindowSize (w, h) = V2 (w * pixelSize) (h * pixelSize)

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
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events

  rendererDrawColor renderer $= V4 255 255 255 255
  runChipCycle renderer chip

  present renderer
  unless qPressed (appLoop renderer chip)

runChipCycle :: (MonadIO m) => Renderer -> IORef Chip -> m ()
runChipCycle renderer chip = do
  ch <- get chip
  chip $~ runCycle
  drawPixels renderer (display ch)

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
