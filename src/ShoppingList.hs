{-# LANGUAGE DeriveGeneric #-}
module ShoppingList where

import           Control.Error.Safe (justErr)
import           Data.Aeson.Types
import           Data.Functor       ((<&>))
import           GHC.Generics
import           KVS                (KVS, listAllKVS, readKV, writeKV)
import           Model
import           Polysemy
import           Polysemy.Error
--import Polysemy.KVStore
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as M
import           MonotonicSequence

newItem :: String -> Int -> Item
newItem productName quantity = Item productName quantity False

addItem :: Members [KVS Id Item, MonotonicSequence Id] r
        => Item
        -> Sem r Id
addItem item = do
  id <- next
  writeKV id item
  return id

listItems :: Member (KVS Id Item) r
          => Sem r (M.Map Id Item)
listItems = fmap M.fromList listAllKVS

getItem :: Members [KVS Id Item, Error AppError] r
        => Id
        -> Sem r Item
getItem id = readKV id >>= \case
  Just item -> pure item
  Nothing -> throw $ ShoppingListItemNotFound id

toggle :: Members [KVS Id Item, Error AppError] r
       => Id
       -> Sem r Item
toggle id = do
  itemErr <- readKV id <&> justErr (ShoppingListItemNotFound id)
  item <- either throw return itemErr
  let completed = _completed item
  let modified = item { _completed = not completed }
  writeKV id modified
  return modified
