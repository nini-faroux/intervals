{-# LANGUAGE NoImplicitPrelude #-}

module Interval where

import RIO
import Say
import Prelude (cycle)

data Interval = Interval { switch :: TVar Switch, interval :: TMVar () }
data Switch = On | Off

runIntervals :: Int -> IO ()
runIntervals numberIntervals = sequenceA_ $ setupIntervals numberIntervals

setupIntervals :: Int -> [IO ()]
setupIntervals numberIntervals = take (numberIntervals * 2) $ cycle [intervalOn, intervalOff]

intervalOn :: IO ()
intervalOn = do
  sayString "Interval start"
  t <- makeInterval onValue
  waitInterval t

intervalOff :: IO ()
intervalOff = do
  sayString "Break start"
  t <- makeInterval offValue
  waitInterval t

onValue :: Int
onValue = 3 * 1000000

offValue :: Int
offValue = 2 * 1000000

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
