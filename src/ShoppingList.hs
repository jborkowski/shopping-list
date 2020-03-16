{-# LANGUAGE DeriveGeneric #-}
module ShoppingList where

import Control.Error.Safe (justErr)
import Data.Functor ((<&>))
import Data.Aeson.Types
import GHC.Generics
import Polysemy
import Polysemy.Error
import KVS
--import Polysemy.KVStore
import MonotonicSequence
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)


type Id = Int

data AppError = ShoppingListItemNotFound Int

data Item = Item { _productName :: String
                 , _quantity :: Int
                 , _bought :: Bool
                 } deriving (Show, Eq, Generic)

instance ToJSON Item
instance FromJSON Item

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
  let bought = _bought item
  let modified = item { _bought = not bought }
  writeKV id modified
  return modified
