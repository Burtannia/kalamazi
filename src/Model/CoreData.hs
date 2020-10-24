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

-- TEXT STUFF:
-- need a way to have links in text
-- allow colour selection on text
    -- presets for class colours, white, grey and the default highlight colour (kala's red I guess) for links etc.
-- worst case just accept HTML with anchor/span tags

data TextBuilder
    = PlainText Text
    | Link (Route App) TextBuilder
    | Highlight TextBuilder -- specific colour for various pieces of text should be set via CSS
    | Bold TextBuilder
    | Italic TextBuilder
    | Underline TextBuilder
    deriving (Show, Read)     

data TextType = Title | Description | Extra | Block | Note
    deriving (Show, Read)

derivePersistField "TextType"