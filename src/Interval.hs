{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Interval where

import RIO
import Say
import Prelude (cycle)
import Sound

data App = App { numberIntervals :: !Int, lengthOn :: !Int, lengthPause :: !Int, filePath :: FilePath }

data Interval = Interval { switch :: TVar Switch, interval :: TMVar () }
data Switch = On | Off deriving Show

runIntervals :: App -> IO ()
runIntervals app = runRIO app setupIntervals >>= \i -> sequenceA_ i

setupIntervals :: RIO App [IO ()]
setupIntervals = do
  App numberIntervals lengthOn lengthPause fp <- ask
  return $ take (numberIntervals * 2) $ cycle [intervalOn fp (lengthOn * t), intervalOff fp (lengthPause * t)]
  where t = 1000000

intervalOn :: FilePath -> Int -> IO ()
intervalOn = intervalSwitch On

intervalOff :: FilePath -> Int -> IO ()
intervalOff = intervalSwitch Off

intervalSwitch :: Switch -> FilePath -> Int -> IO ()
intervalSwitch switch fp time = do
  sayString $ "Interval: " ++ show switch
  intervalSound fp
  t <- makeInterval time
  waitInterval t

stopInterval :: Interval -> IO ()
stopInterval (Interval switch _) = atomically $ writeTVar switch Off

waitInterval :: Interval -> IO ()
waitInterval (Interval _ interval) = atomically $ readTMVar interval

makeInterval :: Int -> IO Interval
makeInterval delay = do
  switch <- newTVarIO On
  interval <- newEmptyTMVarIO
  withAsync (go switch interval) $ \a -> wait a
  return $ Interval switch interval
  where
    go s i = do
      threadDelay delay
      atomically $ do
        switch <- readTVar s
        case switch of
          On -> putTMVar i ()
          Off -> return ()
