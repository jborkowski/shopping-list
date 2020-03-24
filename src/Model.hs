{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Model where

import Data.Aeson.TH
import Data.Aeson.Types

import Database.SQLite.Simple           (FromRow, ToRow, field, fromRow, toRow)
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField   (ToField)

import GHC.Generics


newtype RowId = RowId Int deriving (Eq, Ord, Show, ToField, FromField)

data ItemRow = ItemRow { id        :: Int
                       , name      :: String
                       , quantity  :: Int
                       , completed :: Bool
                       } deriving (Show)

instance FromRow ItemRow where
  fromRow = ItemRow <$> field <*> field <*> field <*> field

instance ToRow ItemRow where
  toRow (ItemRow id_ name quantity completed) = toRow (id_, name, quantity, completed)

toItem :: ItemRow -> (Id, Item)
toItem (ItemRow id_ name quantity completed) = (id_, (Item name quantity completed))

type Id = Int

data AppError = ShoppingListItemNotFound Int

data Item = Item { _name      :: String
                 , _quantity  :: Int
                 , _completed :: Bool
                 } deriving (Show, Eq, Generic)

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Item
