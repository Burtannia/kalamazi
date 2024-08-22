{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
    | CWeakAura WeakAuraId
    | CDivider Axis Bool
    | CTalents TalentConfig
    | CHeroTalents HeroTalentConfig
    deriving (Show, Read)

data HeroTalentConfig = HeroTalentConfig
    { talentClass :: Text
    , talentSpec :: Text
    , talentHero :: Text
    , talentCode :: Text
    , talentExpand :: Bool
    , talentPreview :: ImageId
    }
    deriving (Show, Read, Eq)

data TalentConfig = TalentConfig
    { talentClass :: Text
    , talentSpec :: Text
    , talentCode :: Text
    , talentExpand :: Bool
    , talentPreview :: ImageId
    }
    deriving (Show, Read, Eq)

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

toHeroTalents :: TalentConfig -> HeroTalentConfig
toHeroTalents TalentConfig{talentClass, talentSpec, talentCode, talentExpand, talentPreview} =
    HeroTalentConfig talentClass talentSpec "" talentCode talentExpand talentPreview

derivePersistField "Axis"
derivePersistField "SpaceChar"
derivePersistField "ToggleGroup"
derivePersistField "TalentConfig"
derivePersistField "HeroTalentConfig"
derivePersistField "Component"

share [mkPersist sqlSettings] $(persistFileWith lowerCaseSettings "config/models/guide.persistentmodels")