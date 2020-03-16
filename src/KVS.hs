{-# LANGUAGE TemplateHaskell #-}
module KVS where

import Control.Monad (fmap)
import Polysemy
import Polysemy.State
import qualified Data.Map.Strict as M

data KVS k v m a where
  ListAllKVS :: KVS k v m [(k, v)]
  WriteKV :: k -> v -> KVS k v m ()
  ReadKV :: k -> KVS k v m (Maybe v)
  ModifyKV :: (v -> v) -> k -> KVS k v m ()

makeSem ''KVS

runKvsOnMapState :: (Member (State (M.Map k v)) r, Ord k)
                 => Sem((KVS k v) ': r) a
                 -> Sem r a
runKvsOnMapState = interpret $ \case
  ListAllKVS -> fmap M.toList get
  WriteKV k v -> modify $ M.insert k v
  ReadKV k -> fmap (M.lookup k) get
  ModifyKV fn k -> modify $ M.adjust fn k
