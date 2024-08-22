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
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

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
    } deriving (Show, Read, Eq)

data TalentConfig = TalentConfig
    { talentClass :: Text
    , talentSpec :: Text
    , talentCode :: Text
    , talentExpand :: Bool
    , talentPreview :: ImageId
    } deriving (Show, Read, Eq)

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
toHeroTalents tc = HeroTalentConfig tc.talentClass tc.talentSpec "" tc.talentCode tc.talentExpand tc.talentPreview

derivePersistField "Axis"
derivePersistField "SpaceChar"
derivePersistField "ToggleGroup"
derivePersistField "TalentConfig"
derivePersistField "HeroTalentConfig"
derivePersistField "Component"

share [mkPersist sqlSettings] $(persistFileWith lowerCaseSettings "config/models/guide.persistentmodels")
