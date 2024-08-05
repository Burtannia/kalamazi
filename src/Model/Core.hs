{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE DataKinds #-}
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
module Model.Core where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

import Model.CoreData

share [mkPersist sqlSettings] $(persistFileWith lowerCaseSettings "config/models/core.persistentmodels")

instance Eq Image where
    (==) i1 i2 = (==) (imageUuid i1) (imageUuid i2)
