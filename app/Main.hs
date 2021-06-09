{-# LANGUAGE OverloadedStrings #-}

module Main where

import Chip8
import Foreign.C.Types (CInt)
import SDL
import Linear (V4(..))
import Control.Monad (unless, guard)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Vector.Storable as V
import Data.StateVar
import Data.IORef
import qualified Data.List as L
import Debug.Trace
import System.Environment (getArgs)
import System.Random (getStdGen)

-- This size of a Chip8 "pixel" in ordinary screen pixels
pixelSize :: CInt
pixelSize = 15

getWindowSize :: (CInt, CInt) -> V2 CInt
getWindowSize (w, h) = V2 (w * pixelSize) (h * pixelSize)

-- Scan codes for the Chip8 "keyboard"
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
  -- TODO Proper error handling for missing file
  (fileName:_) <- getArgs
  initializeAll
  window <- createWindow "Chip 8" chip8Window
  renderer <- createRenderer window (-1) defaultRenderer
  program <- loadProgramFromFile fileName
  g <- getStdGen
  chip <- newIORef (initChip program g)

  -- Clear the screen initially
  rendererDrawColor renderer $= V4 0 0 0 0
  clear renderer

  appLoop renderer chip
  destroyWindow window

appLoop :: Renderer -> IORef Chip -> IO ()
appLoop renderer chip = do
  t <- ticks
  events <- pollEvents

  unless (any isExitCode events) $ do
    let extData = ChipExtData { keys = filterChipKeys events
                              , millis = fromIntegral t }
    runChipCycle renderer chip extData

    present renderer
    appLoop renderer chip

isExitCode :: Event -> Bool
isExitCode event = maybe False (== ScancodeEscape) (getScanCode event)

filterChipKeys :: [Event] -> [Int]
filterChipKeys = foldr findChipKey []
  where
    findChipKey event ks =
      maybe ks (:ks) (getScanCode event >>= (`L.elemIndex` scanCodes))

getScanCode :: Event -> Maybe Scancode
getScanCode event =
  case eventPayload event of
    KeyboardEvent keyboardEvent ->
      if keyboardEventKeyMotion keyboardEvent == Pressed
      then Just $ keysymScancode (keyboardEventKeysym keyboardEvent)
      else Nothing
    _ -> Nothing

runChipCycle :: (MonadIO m) => Renderer -> IORef Chip -> ChipExtData -> m ()
runChipCycle renderer chip extData = do
  chip $~ runCycle extData

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
