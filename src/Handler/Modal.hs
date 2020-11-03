{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Modal where

import Import

mkModal :: Text -> (Widget, Enctype) -> Widget
mkModal modalTitle (modalContents, enctype) = do
    modalId <- newIdent
    formId <- newIdent
    $(widgetFile "modal")