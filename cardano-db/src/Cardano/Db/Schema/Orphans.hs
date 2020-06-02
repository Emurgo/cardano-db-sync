{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Db.Schema.Orphans where

import Data.WideWord.Word128 (Word128)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text

import Database.Persist.Class (PersistField (..))
import Database.Persist.Types (PersistValue (..))


instance PersistField Word128 where
  toPersistValue = PersistByteString . BS.pack . show
  fromPersistValue (PersistByteString bs) = Right $ read (BS.unpack bs)
  fromPersistValue x =
    Left $ mconcat [ "Failed to parse Haskell type Word128: ", Text.pack (show x) ]

