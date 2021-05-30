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
  , songs :: [FilePath]
  }

data Switch = On | Off deriving Show

runApp :: App -> IO ()
runApp app = initializeSound >> runRIO app runIntervals

runIntervals :: RIO App ()
runIntervals = do
  App numInts lenOn lenOff ss es songs <- ask
  intervalsVar <- liftIO $ newTMVarIO (numInts * 2, On)
  intervals intervalsVar

intervals :: TMVar (Int, Switch) -> RIO App ()
intervals countVar = do
  App _ lenOn lenOff ss es songs <- ask
  (numIntervals, switch) <- atomically $ readTMVar countVar
  when (numIntervals == 0) (liftIO exitProgram)
  case switch of
    On -> liftIO $ runThreads lenOn ss songs countVar
    Off -> liftIO $ runThreads lenOff es songs countVar
  intervals countVar

runThreads :: Int -> FilePath -> [FilePath] -> TMVar (Int, Switch) -> IO ()
runThreads time file songs intsVar = do
  (numIntervals, switch) <- atomically $ takeTMVar intsVar
  withAsync (go switch) $ \a1 ->
    withAsync (count file time) $ \a2 ->
      withAsync (playSongs songs) $ \a3 -> do
        _ <- wait a1
        _ <- wait a2
        return ()
  atomically $ putTMVar intsVar (numIntervals - 1, toggle switch)
  where 
    go s = sayString (show s) >> playSound file

toggle :: Switch -> Switch
toggle On = Off
toggle _ = On

exitProgram :: IO ()
exitProgram = freeSound >> sayString "Quitting" >> exitSuccess
