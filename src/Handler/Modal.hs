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

mkModalEdit :: Text -> (Widget, Enctype) -> Widget
mkModalEdit t f = mkModalCustom t f defs
    where
        defs = ModalSettings [shamlet| <i .lnir .lnir-pencil> |] "btn btn-dark"

mkModalCustom :: Text
    -> (Widget, Enctype)
    -> ModalSettings
    -> Widget
mkModalCustom modalTitle (modalContents, enctype) ModalSettings {..} = do
    modalId <- newIdent
    formId <- newIdent
    $(widgetFile "modal")