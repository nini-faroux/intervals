module Counter where

import RIO
import Control.Monad (replicateM)
import Say
import Sound

type Counter = TMVar Int

makeCounter :: Int -> IO Counter
makeCounter = newTMVarIO

count :: FilePath -> Int -> String -> IO ()
count fp n switch = makeCounter n >>= \counter -> 
  replicateM_ n $ concurrently (decrement counter) (warningSound fp counter switch)

decrement :: Counter -> IO ()
decrement counterVar = do
  threadDelay 1000000
  c <- atomically $ do
    count <- takeTMVar counterVar
    putTMVar counterVar $! count - 1
    readTMVar counterVar
  sayString $ show c

warningSound :: FilePath -> Counter -> String -> IO ()
warningSound fp counterVar switch = do
  n <- atomically $ readTMVar counterVar
  when (n == 10 && switch == "On") (playSound fp)
