{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module MonotonicSequence where

import           Control.Monad          ((>>))
import           Data.Functor           ((<$>))
import           Data.Maybe             (listToMaybe, maybe)
import           Database.SQLite.Simple (NamedParam ((:=)))
import           Database.SQLite.Simple (FromRow, ToRow, field, fromRow, toRow, Only)
import qualified Database.SQLite.Simple as SQL
import           Model                  (Id, Item (..), ItemRow (..), toItem, RowId)
import           Polysemy               (Embed, Member, Sem)
import qualified Polysemy               as P
import qualified Polysemy.Input         as PI
import qualified Polysemy.State         as PS


data MonotonicSequence v m a where
  Next :: MonotonicSequence v m v

P.makeSem ''MonotonicSequence

runMonotonicSequenceOnState :: ( Member (PS.State v) r, Num v)
                            => Sem ((MonotonicSequence v) ': r) a
                            -> Sem r a
runMonotonicSequenceOnState = P.interpret $ \case
  Next -> PS.modify (+1) >> PS.get

runMonotonicSequenceOnSQLite :: Member (Embed IO) r
                             => Sem ((MonotonicSequence Id) : r) a
                             -> Sem (PI.Input SQL.Connection : r) a
runMonotonicSequenceOnSQLite = P.reinterpret $ \case
  Next -> do
    conn <- PI.input
    lastId <- P.embed $ SQL.query_ conn "SELECT MAX(id) FROM products"
    return (maybe 0 (+1) $ SQL.fromOnly <$> listToMaybe lastId)
