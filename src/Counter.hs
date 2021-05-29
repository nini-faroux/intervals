module Counter where

import RIO
import Control.Monad (replicateM)
import Say
import Sound

type Counter = TMVar Int

makeCounter :: Int -> IO Counter
makeCounter = newTMVarIO

count :: FilePath -> Int -> IO ()
count fp n = makeCounter n >>= \counter -> 
  replicateM_ n $ concurrently (decrement counter) (warningSound fp counter)

decrement :: Counter -> IO ()
decrement counterVar = do
  threadDelay 1000000
  c <- atomically $ do
    count <- takeTMVar counterVar
    putTMVar counterVar $! count - 1
    readTMVar counterVar
  sayString $ show c

warningSound :: FilePath -> Counter -> IO ()
warningSound fp counterVar = do
  n <- atomically $ readTMVar counterVar
  when (n == 10) (playSound fp)
