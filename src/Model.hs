{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Model
    ( module Model
    ) where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
    
import Model.Core as Model
import Model.CoreData as Model
import Model.Guide as Model

share [mkMigrate "migrateAll"]
    $(persistManyFileWith lowerCaseSettings
        [ "config/models/core.persistentmodels"
        , "config/models/guide.persistentmodels"
        ])