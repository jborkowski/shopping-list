{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Function ((&))
import Data.IORef
import KVS
import MonotonicSequence
import ShoppingList
import ShoppingListApp
import Polysemy
import Polysemy.Error
import Polysemy.State
import Servant.Server
import Control.Monad.Except
import Options.Generic

import qualified Data.Map.Strict as M
import qualified Network.Wai.Handler.Warp as W

initState :: M.Map Id Item
initState = M.singleton 1 (newItem "Milk" 2)

createApp :: IO Application
createApp = do
  idIORef <- newIORef 0
  kvsIORef <- newIORef initState
  return (serve api $ hoistServer api (\sem -> interpretServer sem idIORef kvsIORef) server)

  where
    interpretServer sem idIORef kvsIORef = sem
      & runKvsOnMapState
      & runMonotonicSequenceOnState
      & runStateIORef @Int idIORef
      & runStateIORef @(M.Map Id Item) kvsIORef
      & runError @AppError
      & runM
      & liftToHandler
    liftToHandler = Handler . ExceptT . (fmap handleErrors)
    handleErrors (Left (ShoppingListItemNotFound id)) = Left err404 {errBody = "Item with provided ID does not exists"}
    handleErrors (Right value) = Right value

data Args = Args { port :: Int } deriving (Show, Generic)
instance ParseRecord Args


main :: IO ()
main = do
  Args(port) <- getRecord "Shopping List -- Server"
  app <- createApp
  W.run port app
