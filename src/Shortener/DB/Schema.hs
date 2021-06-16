{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, GADTs,
  StandaloneDeriving, UndecidableInstances, FlexibleInstances,
  MultiParamTypeClasses #-}
module Shortener.DB.Schema where

import Database.Persist.TH
import Shortener.API

$(share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Url
  shortId ShortId
  fullUrl FullUrl
  UniqueShortId shortId
|])
