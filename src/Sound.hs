module Sound where

import RIO
import qualified SDL.Mixer as Mix
import qualified SDL

import Control.Monad      (when)
import Data.Default.Class (def)

initializeSound :: IO ()
initializeSound = do
  SDL.initialize [SDL.InitAudio]
  Mix.initialize [Mix.InitFLAC]
  Mix.openAudio def 256

playSound :: FilePath -> IO ()
playSound fp = Mix.load fp >>= \s -> Mix.play s

playSongs :: [FilePath] -> IO ()
playSongs = mapM_ (Mix.load >=> \s -> Mix.play s)

freeSound :: IO ()
freeSound = do
  Mix.closeAudio
  Mix.quit
  SDL.quit
