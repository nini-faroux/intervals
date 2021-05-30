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
playSongs = mapM_ (Mix.load >=> \s -> playSong s)

playSong :: Mix.Chunk -> IO ()
playSong s = Mix.play s >> loopSound (Mix.playing Mix.AllChannels)

loopSound :: IO Bool -> IO ()
loopSound ioCond = ioCond >>= \cond -> when cond (loopSound ioCond)

freeSound :: IO ()
freeSound = do
  Mix.closeAudio
  Mix.quit
  SDL.quit
