{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Db.AlterTable (
  AlterTable(..),
  DbAlterTableException(..),
  alterTable,
) where

import Control.Exception.Lifted (Exception, handle, throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Data.Text as T
import Database.Persist.Postgresql (ConstraintNameDB (..), EntityNameDB (..), FieldNameDB (..), SqlBackend, rawExecute, EntityDef)
import Database.PostgreSQL.Simple (SqlError)
import Database.Persist.EntityDef.Internal (entityDB)

-- The ability to `ALTER TABLE` currently dealing with `CONSTRAINT` but can be extended
data AlterTable
  = AddUniqueConstraint ConstraintNameDB [FieldNameDB]
  | DropUniqueConstraint ConstraintNameDB
  deriving (Show)

data DbAlterTableException
  = DbAlterTableException String SqlError
  deriving (Show)

instance Exception DbAlterTableException

alterTable ::
  forall m.
  ( MonadBaseControl IO m
  , MonadIO m
  ) =>
  EntityDef ->
  AlterTable ->
  ReaderT SqlBackend m ()
alterTable entity (AddUniqueConstraint cname cols) =
  handle alterTableExceptHandler (rawExecute query [])
  where
    query :: T.Text
    query =
      T.concat
        [ "ALTER TABLE "
        , unEntityNameDB (entityDB entity)
        , " ADD CONSTRAINT "
        , unConstraintNameDB cname
        , " UNIQUE("
        , T.intercalate "," $ map escapeDBName' cols
        , ")"
        ]
    escapeDBName' :: FieldNameDB -> T.Text
    escapeDBName' name = unFieldNameDB name
alterTable entity (DropUniqueConstraint cname) =
  handle alterTableExceptHandler (rawExecute query [])
  where
    query :: T.Text
    query =
      T.concat
        [ "ALTER TABLE "
        , unEntityNameDB (entityDB entity)
        , " DROP CONSTRAINT "
        , unConstraintNameDB cname
        ]

alterTableExceptHandler ::
  forall m a.
  MonadIO m =>
  SqlError ->
  ReaderT SqlBackend m a
alterTableExceptHandler e = liftIO $ throwIO (DbAlterTableException "" e)
