module Interval where

import RIO
import Say
import Prelude (cycle)
import Sound
import Counter

data App = App 
  { numIntervals :: !Int 
  , lengthOn :: !Int
  , lengthPause :: !Int
  , startSoundFile :: !FilePath
  , endSoundFile :: !FilePath 
  }

data Switch = On | Off deriving Show

runApp :: App -> IO ()
runApp app = initializeSound >> runRIO app runIntervals

runIntervals :: RIO App ()
runIntervals = do
  App numInts lenOn lenOff ss es <- ask
  intervalsVar <- liftIO $ newTMVarIO (numInts * 2, On)
  intervals intervalsVar

intervals :: TMVar (Int, Switch) -> RIO App ()
intervals countVar = do
  App _ lenOn lenOff ss es <- ask
  (numIntervals, switch) <- atomically $ readTMVar countVar
  when (numIntervals == 0) (liftIO exitProgram)
  case switch of
    On -> liftIO $ runThreads lenOn ss countVar
    Off -> liftIO $ runThreads lenOff es countVar
  atomically $ readTMVar countVar
  intervals countVar

runThreads :: Int -> FilePath -> TMVar (Int, Switch) -> IO ()
runThreads time file intsVar = do
  (numIntervals, switch) <- atomically $ takeTMVar intsVar
  (_, _) <- concurrently (go switch) (count file time)
  atomically $ putTMVar intsVar (numIntervals - 1, toggle switch)
  where 
    go s = sayString (show s) >> playSound file

toggle :: Switch -> Switch
toggle On = Off
toggle _ = On

exitProgram :: IO ()
exitProgram = freeSound >> sayString "Quitting" >> exitSuccess
