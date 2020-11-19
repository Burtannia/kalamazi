{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Modal where

import Import
import Text.Julius (rawJS)

data ModalSettings = ModalSettings
    { msButtonInner :: Html
    , msButtonClass :: Text
    }

mkModal :: Text -> (Widget, Enctype) -> Widget
mkModal t f = mkModalCustom t f defs
    where
        defs = ModalSettings [shamlet| #{t} |] "btn btn-primary"

mkModalCustom :: Text
    -> (Widget, Enctype)
    -> ModalSettings
    -> Widget
mkModalCustom modalTitle (modalContents, enctype) ModalSettings {..} = do
    modalId <- newIdent
    formId <- newIdent
    $(widgetFile "modal")