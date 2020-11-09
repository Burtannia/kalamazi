{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Component where

import Import
import Handler.Images
import Handler.Modal
import Summernote
import Data.Maybe (fromJust)
import           Text.Julius                     (rawJS)
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), renderBootstrap4)

import Model.Guide

compForm :: AForm Handler ComponentType
compForm = areq (radioField $ optionsPairs components) "Component Type" Nothing
    where
        components :: [(Text, ComponentType)]
        components = [("Markup", CT_Markup)]

data ComponentType = CT_Markup -- | CT_Toggle
    deriving (Show, Read, Eq)

createComponent :: ComponentType -> Handler Component
createComponent CT_Markup = liftM CMarkup $
    runDB $ insert $ Markup $ preEscapedToMarkup ("Click to edit..." :: Text)

displayComponent :: SectionId -> Int -> Component -> Widget
displayComponent sectionId cIx (CMarkup markupId) = do
    markup <- handlerToWidget $ runDB $ getJust markupId
    contentId <- newIdent
    saveId <- newIdent
    deleteId <- newIdent
    addStylesheet $ StaticR summernote_summernotebs4_css
    addScript $ StaticR summernote_summernotebs4_js
    $(widgetFile "components/markup")