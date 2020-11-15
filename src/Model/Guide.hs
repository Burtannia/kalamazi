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

data Component = CMarkup MarkupId | CToggle ToggleGroup
    deriving (Show, Read)

data SpaceChar = SpaceLine | SpaceChev
    deriving (Show, Read, Eq)

type ToggleOption selector = (selector, MarkupId)

data ToggleGroup
    = ToggleTexts SpaceChar [ToggleOption Text]
    | ToggleImages ImageId [ToggleOption ImageId]
    deriving (Show, Read)

-- A form friendly representation of a component to be constructed.
data CreateComponent
    = CreateMarkup Html
    | CreateToggleText SpaceChar [(Text, Html)]
    | CreateToggleImage (Entity Image) [(Entity Image, Html)]

type Grid = [Row]
type Row = [Column]
type Column = [Component]

derivePersistField "SpaceChar"
derivePersistField "ToggleGroup"
derivePersistField "Component"

share [mkPersist sqlSettings] $(persistFileWith lowerCaseSettings "config/models/guide.persistentmodels")