{-# LANGUAGE DeriveGeneric, FlexibleInstances #-}
module Model where

import Yesod
import Data.Text (Text)
import Data.Int (Int64)
import Database.Persist.Quasi
import Prelude
import Data.Aeson.Types
import Data.Aeson.Encode (encodeToTextBuilder)
import Text.Julius
import GHC.Generics

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance ToJSON Swag where
    toJSON = genericToJSON defaultOptions

instance ToJavascript Swag where
    toJavascript = Javascript . encodeToTextBuilder . toJSON

instance ToJavascript [Swag] where
    toJavascript = Javascript . encodeToTextBuilder . toJSON
