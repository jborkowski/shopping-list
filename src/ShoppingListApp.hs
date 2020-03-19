module ShoppingListApp where

import qualified Data.Map.Strict   as M
import           Data.Proxy
import           KVS
import           MonotonicSequence
import           Polysemy
import           Polysemy.Error
import           Servant           ((:<|>), (:>), Capture, Get, JSON, Post,
                                    ReqBody, ServerT)
import           Servant
import           ShoppingList


type ShoppingListAPI
   =    Get '[JSON] (M.Map Int Item)
   :<|> Capture "id" Int :> Get '[JSON] Item
   :<|> Capture "id" Int :> "toggle" :> Get '[JSON] Item
   :<|> ReqBody '[JSON] Item :> Post '[JSON] Item

api :: Proxy ShoppingListAPI
api = Proxy

server :: Members [KVS Id Item, Error AppError, MonotonicSequence Id] r
       => ServerT ShoppingListAPI (Sem r)
server =
       listItems
  :<|> getItem
  :<|> toggle
  :<|> addAndGet

  where
    addAndGet item = (addItem item) >>= getItem
