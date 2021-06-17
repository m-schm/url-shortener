{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, GADTs,
  StandaloneDeriving, UndecidableInstances, FlexibleInstances,
  DataKinds #-}
module Shortener.DB.Schema where

import Database.Persist.TH
import Shortener.Types

$(share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Url
  shortId ShortId
  fullUrl FullUrl
  UniqueShortId shortId
|])
