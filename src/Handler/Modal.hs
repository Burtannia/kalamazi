{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Modal where

import Import

data ModalSettings = ModalSettings
    { msButtonInner :: Html
    , msButtonClass :: Text
    }

mkModal :: Text -> (Widget, Enctype) -> Widget
mkModal t f = mkModalCustom t f defs
    where
        defs = ModalSettings [shamlet| #{t} |] "btn btn-dark"

mkModalAdd :: Text -> (Widget, Enctype) -> Widget
mkModalAdd t f = mkModalCustom t f defs
    where
        defs = ModalSettings [shamlet| <i .lnir .lnir-plus> |] "btn btn-dark"

editSettings :: ModalSettings
editSettings = ModalSettings
    [shamlet| <i .lnir .lnir-pencil> |] "btn btn-dark"

mkModalEdit :: Text -> (Widget, Enctype) -> Widget
mkModalEdit t f = mkModalCustom t f editSettings

mkModalEditHtmx :: Text -> SectionId -> Int -> Text -> Widget
mkModalEditHtmx title sectionId compIx compFormId =
    mkModalHtmx title sectionId compIx compFormId editSettings

mkModalCustom :: Text
    -> (Widget, Enctype)
    -> ModalSettings
    -> Widget
mkModalCustom modalTitle (modalContents, enctype) ModalSettings {..} = do
    modalId <- newIdent
    formId <- newIdent
    $(widgetFile "modal")

mkModalHtmx :: Text
    -> SectionId
    -> Int
    -> Text
    -> ModalSettings
    -> Widget
mkModalHtmx modalTitle sectionId compIx compFormId ModalSettings {..} = do
    modalId <- newIdent
    contentId <- newIdent
    $(widgetFile "modal-htmx")
