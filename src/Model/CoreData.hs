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
module Model.CoreData where

import ClassyPrelude.Yesod

-- allow colour selection on text
    -- presets for class colours, white, grey and the default highlight colour (kala's red I guess) for links etc.

data ImageExt = JPG | PNG | GIF
    deriving (Show, Read)

data MarkupCompType = Title | Description | Extra | Block | Note
    deriving (Show, Read)

derivePersistField "ImageExt"
derivePersistField "MarkupCompType"