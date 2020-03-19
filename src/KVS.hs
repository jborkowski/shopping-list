{-# LANGUAGE TemplateHaskell #-}
module KVS where

import           Control.Monad          (fmap)
import qualified Data.Map.Strict        as M
import           Database.SQLite.Simple (NamedParam ((:=)))
import qualified Database.SQLite.Simple as SQL
import           Polysemy               (Embed, Member, Sem)
import qualified Polysemy               as P
import qualified Polysemy.Input         as PI
import           Polysemy.State         as PS


data KVS k v m a where
  ListAllKVS :: KVS k v m [(k, v)]
  WriteKV :: k -> v -> KVS k v m ()
  ReadKV :: k -> KVS k v m (Maybe v)
  ModifyKV :: (v -> v) -> k -> KVS k v m ()

P.makeSem ''KVS

runKvsOnMapState :: (Member (State (M.Map k v)) r, Ord k)
                 => Sem((KVS k v) ': r) a
                 -> Sem r a
runKvsOnMapState = P.interpret $ \case
  ListAllKVS -> fmap M.toList PS.get
  WriteKV k v -> PS.modify $ M.insert k v
  ReadKV k -> fmap (M.lookup k) PS.get
  ModifyKV fn k -> PS.modify $ M.adjust fn k

runKvsAsSQLite :: Member (Embed IO) r
               => Sem ((KVS k v) : r) a
               -> Sem (PI.Input SQL.Connection : r) a
runKvsAsSQLite = P.reinterpret $ \case
  ListAllKVS -> do
    conn <- PI.input
    undefined
   -- products <- P.embed $ SQL.queryNamed conn
   --             "SELECT * FROM products"
   --             []
   --  return (SQL.fromOnly $ products)
  WriteKV k v -> undefined
  ReadKV k -> unefined
  ModifyKV fn k -> undefined
