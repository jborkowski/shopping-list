{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module KVS (
  KVS
  , readKV
  , writeKV
  , listAllKVS
  , runKvsOnMapState
  , runKvsOnSQLite
  ) where

import           Control.Monad          (fmap)
import           Data.Functor           ((<$>))
import qualified Data.Map.Strict        as M
import           Data.Maybe             (listToMaybe)
import           Database.SQLite.Simple (NamedParam ((:=)))
import           Database.SQLite.Simple (FromRow, ToRow, field, fromRow, toRow)
import qualified Database.SQLite.Simple as SQL
import           Model                  (Id, Item(..), toItem, ItemRow(..))
import           Polysemy               (Embed, Member, Sem)
import qualified Polysemy               as P
import qualified Polysemy.Input         as PI
import           Polysemy.State         as PS



data KVS k v m a where
  ListAllKVS :: KVS k v m [(k, v)]
  WriteKV :: k -> v -> KVS k v m ()
  ReadKV :: k -> KVS k v m (Maybe v)
  --ModifyKV :: (v -> v) -> k -> KVS k v m ()

P.makeSem ''KVS

runKvsOnMapState :: (Member (State (M.Map k v)) r, Ord k)
                 => Sem((KVS k v) ': r) a
                 -> Sem r a
runKvsOnMapState = P.interpret $ \case
  ListAllKVS -> fmap M.toList PS.get
  WriteKV k v -> PS.modify $ M.insert k v
  ReadKV k -> fmap (M.lookup k) PS.get
  --ModifyKV fn k -> PS.modify $ M.adjust fn k

runKvsOnSQLite :: Member (Embed IO) r
               => Sem ((KVS Id Item) : r) a
               -> Sem (PI.Input SQL.Connection : r) a
runKvsOnSQLite = P.reinterpret $ \case
  ListAllKVS -> do
    conn <- PI.input
    products <- P.embed $ SQL.queryNamed conn "SELECT id, product, quantity, completed FROM products WHERE completed = :completed" [":completed" := False]
    return (toItem <$> products)

  WriteKV id (Item productName quantity completed) -> do
    conn <- PI.input
    P.embed $ SQL.executeNamed conn
      "INSERT INTO products (id, product, quantity, completed) VALUES (:id, :product, :quantity, :completed)" -- <>
      --"ON CONFLICT (id) DO UPDATE SET product = excluded.product, quantity = excluded.quantity, completed = excluded.completed"
      [":id" := id, ":product" := productName, ":quantity" := quantity, ":completed" := completed]

  ReadKV id -> do
    conn <- PI.input
    products <- P.embed $ SQL.queryNamed conn
      "SELECT id, product, quantity, completed FROM products WHERE id = :id"
      [":id" := id]
    return (snd . toItem <$> listToMaybe products)

--  ReadKV k -> unefined
