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
module Model.Guide where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

import Model.Core

data Component
    = TextComp TextComponentId
    | ImageComp UploadImageId
    | BackgroundComp UploadImageId
    | ToggleComp ToggleGroup
    | Grid GridOpts [Row]
    deriving (Show, Read)

data SpaceChar = SpaceLine | SpaceChev
    deriving (Show, Read)

type ToggleOption selector = (selector, [Component])

data ToggleGroup
    = ToggleTexts SpaceChar [ToggleOption Text]
    | ToggleImages UploadImageId [ToggleOption UploadImageId]
    deriving (Show, Read)

data GridOpts = GridOpts
    { goRuleV :: Bool
    , goRuleH :: Bool
    } deriving (Show, Read)

type Row = [Component]

derivePersistField "SpaceChar"
derivePersistField "ToggleGroup"
derivePersistField "GridOpts"
derivePersistField "Component"

share [mkPersist sqlSettings] $(persistFileWith lowerCaseSettings "config/models/guide.persistentmodels")