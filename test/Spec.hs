{-# LANGUAGE
    ScopedTypeVariables
  #-}

import Control.Monad (forever)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TSetChan (TSetChan, newTSetChan, openTSetChan, closeTSetChan, writeTSetChan, readTSetChan)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM.TSem (newTSem, waitTSem, signalTSem)

main :: IO ()
main = do
  (xs :: TSetChan Int) <- atomically newTSetChan
  c1 <- atomically $ openTSetChan xs
  c2 <- atomically $ openTSetChan xs

  q1 <- atomically $ newTSem 0
  q2 <- atomically $ newTSem 0

  _ <- async $ forever $ do
    x <- atomically $ readTSetChan xs c1
    print x
    atomically $ signalTSem q1
  _ <- async $ forever $ do
    x <- atomically $ readTSetChan xs c2
    print x
    atomically $ signalTSem q2
  atomically $ writeTSetChan xs 1

  atomically $ do
    waitTSem q1
    waitTSem q2
