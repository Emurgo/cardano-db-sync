{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Cardano.Db.AlterTable (
  AlterTable (..),
  DbAlterTableException (..),
  alterTable,
) where

import Control.Exception.Lifted (Exception, handle, throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Data.Text as T
import Database.Persist.EntityDef.Internal (entityDB)
import Database.Persist.Postgresql (ConstraintNameDB (..), EntityDef, EntityNameDB (..), FieldNameDB (..), Single (..), SqlBackend, fieldDB, getEntityFields, rawExecute, rawSql)
import Database.PostgreSQL.Simple (ExecStatus (..), SqlError (..))

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
  , MonadFail m
  ) =>
  EntityDef ->
  AlterTable ->
  ReaderT SqlBackend m ()
alterTable entity (AddUniqueConstraint cname cols) = do
  -- Check that entity fields are in the schema
  if checkAllFieldsValid entity cols
    then do
      -- check if the constraint name already exists
      constraintRes <- queryConstraint cname
      if constraintRes == "1"
        then throwErr "Constraint field already exist"
        else -- if it doesn't exist then add a new constraint
          handle alterTableExceptHandler (rawExecute queryAddConstraint [])
    else error "Some of the unique values which you are being added to the constraint don't correlate"
  where
    queryAddConstraint :: T.Text
    queryAddConstraint =
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

    throwErr :: forall m'. (MonadIO m') => [Char] -> ReaderT SqlBackend m' ()
    throwErr e = liftIO $ throwIO (DbAlterTableException e sqlError)

    queryCheckConstraint :: T.Text
    queryCheckConstraint =
      T.concat
        [ "SELECT COUNT(*) FROM pg_constraint WHERE conname ='"
        , unConstraintNameDB cname
        , "'"
        ]
alterTable entity (DropUniqueConstraint cname) =
  handle alterTableExceptHandler (rawExecute query [])
  where
    query :: T.Text
    query =
      T.concat
        [ "ALTER TABLE "
        , unEntityNameDB (entityDB entity)
        , " DROP CONSTRAINT IF EXISTS "
        , unConstraintNameDB cname
        ]

-- check if a constraint is already present
queryConstraint ::
  ( MonadIO m
  , MonadFail m
  ) =>
  ConstraintNameDB ->
  ReaderT SqlBackend m T.Text
queryConstraint cname = do
  let query = T.concat ["SELECT 1 FROM pg_constraint WHERE conname ='", unConstraintNameDB cname, "'"]
  [Single constraintNum] <- rawSql query []
  pure constraintNum

-- check to see that the field inputs exist
checkAllFieldsValid :: Foldable t => EntityDef -> t FieldNameDB -> Bool
checkAllFieldsValid entity cols = do
  let fieldDef = getEntityFields entity
      fieldDbs = map fieldDB fieldDef
  all (`elem` fieldDbs) cols

alterTableExceptHandler ::
  forall m a.
  MonadIO m =>
  SqlError ->
  ReaderT SqlBackend m a
alterTableExceptHandler e = liftIO $ throwIO (DbAlterTableException "" e)

sqlError :: SqlError
sqlError =
  SqlError
    { sqlState = ""
    , sqlExecStatus = FatalError
    , sqlErrorMsg = ""
    , sqlErrorDetail = ""
    , sqlErrorHint = ""
    }
