{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Interval where

import RIO
import Say
import Prelude (cycle)

data App = App { numberIntervals :: !Int, lengthOn :: !Int, lengthPause :: !Int }

data Interval = Interval { switch :: TVar Switch, interval :: TMVar () }
data Switch = On | Off

runIntervals :: App -> IO ()
runIntervals app = runRIO app setupIntervals >>= \i -> sequenceA_ i

setupIntervals :: RIO App [IO ()]
setupIntervals = do
  App numberIntervals lengthOn lengthPause <- ask
  return $ take (numberIntervals * 2) $ cycle [intervalOn $ lengthOn * t, intervalOff $ lengthPause * t]
  where t = 1000000

intervalOn :: Int -> IO ()
intervalOn on = do
  sayString "Interval start"
  t <- makeInterval on
  waitInterval t

intervalOff :: Int -> IO ()
intervalOff off = do
  sayString "Break start"
  t <- makeInterval off
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
