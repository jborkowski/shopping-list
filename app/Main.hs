{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.Except
import           Data.Function            ((&))
import           Data.IORef
import qualified Data.Map.Strict          as M
import qualified Database.SQLite.Simple   as SQL
import           KVS                      (KVS (..), runKvsOnSQLite)
import           Model                    (AppError (ShoppingListItemNotFound),
                                           Id, Item (..))
import           MonotonicSequence        (MonotonicSequence (..),
                                           runMonotonicSequenceOnSQLite)
import qualified Network.Wai.Handler.Warp as W
import           Options.Generic
import           Polysemy                 (Members, Sem)
import qualified Polysemy                 as P
import           Polysemy.Error           (Error (..))
import qualified Polysemy.Error           as PE
import qualified Polysemy.Input           as PI
import qualified Polysemy.State           as PS
import           Servant.Server
import           ShoppingList
import           ShoppingListApp

createApp :: SQL.Connection -> IO Application
createApp conn = do
  idIORef <- newIORef 0
  return (serve api $ hoistServer api (\sem -> interpret conn sem) server)

  where
    interpret conn sem = sem
      & runKvsOnSQLite
      & PI.runInputConst conn
      & runMonotonicSequenceOnSQLite
      & PI.runInputConst conn
      & PE.runError @AppError
      & P.runM
      & liftToHandler
    liftToHandler = Handler . ExceptT . (fmap handleErrors)
    handleErrors (Left (ShoppingListItemNotFound id)) = Left err404 {errBody = "Item with provided ID does not exists"}
    handleErrors (Right value) = Right value


data Args = Args { port :: Int, dbFile :: String } deriving (Show, Generic)
instance ParseRecord Args

withDbConnection :: String -> (SQL.Connection -> IO a) -> IO a
withDbConnection dbFile fn = SQL.withConnection dbFile $ \conn -> do
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS products (id INTEGER PRIMARY KEY, product text, quantity INTEGER, completed BOOL)"
  fn conn

main :: IO ()
main = do
  (Args port dbFile) <- getRecord "Shopping List -- Server"
  app <- withDbConnection dbFile $ createApp
  W.run port app
