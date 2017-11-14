{-# LANGUAGE
    RecordWildCards
  , NamedFieldPuns
  #-}

module Control.Concurrent.STM.TSetChan
  ( TSetChan, ChanID, newTSetChan, openTSetChan, closeTSetChan, writeTSetChan, readTSetChan
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TVar (TVar, modifyTVar, readTVar, newTVar, writeTVar)
import Control.Concurrent.STM.TChan (TChan, newBroadcastTChan, dupTChan, newTChan, writeTChan, readTChan)



newtype ChanID = ChanID {getChanID :: Int}


data TSetChan a = TSetChan
  { tSetChanContent :: TVar (IntMap (TChan a))
  , tSetChanBroadcast :: (TChan a)
  , tSetChanNonce :: TVar Int
  }


newTSetChan :: STM (TSetChan a)
newTSetChan = do
  bCast <- newBroadcastTChan
  content <- newTVar IntMap.empty
  counter <- newTVar minBound
  pure TSetChan
    { tSetChanContent = content
    , tSetChanBroadcast = bCast
    , tSetChanNonce = counter
    }


openTSetChan :: TSetChan a -> STM ChanID
openTSetChan TSetChan{..} = do
  newChan <- dupTChan tSetChanBroadcast
  nonce <- do
    x <- readTVar tSetChanNonce
    writeTVar tSetChanNonce (x+1)
    pure x
  xs <- readTVar tSetChanContent
  writeTVar tSetChanContent (IntMap.insert nonce newChan xs)
  pure (ChanID nonce)


writeTSetChan :: TSetChan a -> a -> STM ()
writeTSetChan TSetChan{tSetChanBroadcast} x = writeTChan tSetChanBroadcast x


readTSetChan :: TSetChan a -> ChanID -> STM (Maybe a)
readTSetChan TSetChan{tSetChanContent} (ChanID nonce) = do
  xs <- readTVar tSetChanContent
  case IntMap.lookup nonce xs of
    Nothing -> pure Nothing
    Just chan -> Just <$> readTChan chan


closeTSetChan :: TSetChan a -> ChanID -> STM ()
closeTSetChan TSetChan{tSetChanContent} (ChanID nonce) = modifyTVar tSetChanContent (IntMap.delete nonce)
