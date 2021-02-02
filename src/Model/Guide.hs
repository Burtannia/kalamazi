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

data Axis = Column | Row
    deriving (Show, Read, Eq)

data Component
    = CMarkup MarkupBlockId
    | CToggle ToggleGroup
    | CImage ImageId
    | CVideo Text
    | CWeakAura Text Textarea
    | CDivider Axis Bool
    deriving (Show, Read)

data SpaceChar = SpaceLine | SpaceChev
    deriving (Show, Read, Eq)

type ToggleOption selector = (selector, MarkupBlockId)

data ToggleGroup
    = ToggleTexts SpaceChar [ToggleOption Text]
    | ToggleImages [ToggleOption ImageId]
    deriving (Show, Read)

type Grid = [Row]
type Row = [Column]
type Column = [Component]

derivePersistField "Axis"
derivePersistField "SpaceChar"
derivePersistField "ToggleGroup"
derivePersistField "Component"

share [mkPersist sqlSettings] $(persistFileWith lowerCaseSettings "config/models/guide.persistentmodels")