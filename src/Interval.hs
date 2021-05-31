module Interval where

import RIO
import Say
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
  countVar <- liftIO $ newTMVarIO (numInts * 2, On)
  concurrently' (intervals countVar) (liftIO $ playSongs songs)

intervals :: TMVar (Int, Switch) -> RIO App ()
intervals countVar = do
  App _ lenOn lenOff ss es songs <- ask
  (numIntervals, _) <- atomically $ readTMVar countVar
  when (numIntervals == 0) (liftIO exitProgram)
  runThreads countVar
  intervals countVar

runThreads :: TMVar (Int, Switch) -> RIO App ()
runThreads countVar = do
  App _ lenOn lenOff ss es songs <- ask
  (numIntervals, switch) <- atomically $ takeTMVar countVar
  case switch of
    On -> conc On ss lenOn
    Off -> conc Off es lenOff 
  atomically $ putTMVar countVar (numIntervals - 1, toggle switch)
  where
    conc s fp time = concurrently' (liftIO $ go s fp) (liftIO $ count fp time)
    go s fp = sayString (show s) >> playSound fp

concurrently' :: RIO App a -> RIO App b -> RIO App ()
concurrently' x y = withRunInIO $ \run -> concurrently_ (run x) (run y)

toggle :: Switch -> Switch
toggle On = Off
toggle _ = On

exitProgram :: IO ()
exitProgram = freeSound >> sayString "Quitting..." >> threadDelay 2000000 >> exitSuccess
