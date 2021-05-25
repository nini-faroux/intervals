module Sound where

import RIO
import qualified SDL.Mixer as Mix
import qualified SDL

import Control.Monad      (when)
import Data.Default.Class (def)

intervalSound :: FilePath -> IO ()
intervalSound fp = do
  SDL.initialize [SDL.InitAudio]
  Mix.initialize [Mix.InitFLAC]
  Mix.openAudio def 256
  sound <- Mix.load fp
  Mix.play sound
  loopSound $ Mix.playing Mix.AllChannels

  Mix.free sound
  Mix.closeAudio
  Mix.quit
  SDL.quit

loopSound :: Monad m => m Bool -> m ()
loopSound condM = do
  cond <- condM
  when cond $ loopSound condM
