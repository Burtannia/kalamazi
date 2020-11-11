{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Component where

import Import
import Handler.Images
import Handler.Modal
import Summernote
import Data.List (foldr1)
import Data.Maybe (fromJust)
import Text.Julius (rawJS)
import Data.Aeson.Types
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), renderBootstrap4)

import Model.Guide

compForm :: Form ComponentType
compForm extra = do
    images <- lift $ runDB getAllImages

    let markupList = [("Markup" :: Text, CT_Markup)]
        markupSettings = "Markup" {fsName = Just "compField"}
        markupField = radioField $ return $ mkOptions "markup" markupList

        textList =
            [ ("Toggle Group |" :: Text, CT_Toggle_Text SpaceLine)
            , ("Toggle Group >", CT_Toggle_Text SpaceChev) ]
        textSettings = "Text Toggle" {fsName = Just "compField"}
        textField = radioField $ return $ mkOptions "text" textList
        
        mkImageOpt e = (imageName $ entityVal e, CT_Toggle_Image $ entityKey e)
        imageList = map mkImageOpt images
        imageSettings = "Image Toggle" {fsName = Just "compField"}
        imageField = imageSelectField -- radioField $ return $ mkOptions "image" imageList

    (markupRes, markupView) <- mreq markupField markupSettings Nothing
    (textRes, textView) <- mreq textField textSettings Nothing
    (imageRes, imageView) <- mreq imageField imageSettings Nothing

    let view =
            [whamlet|
                #{extra}
                <div> Markup:
                ^{fvInput markupView}
                <div> Text Toggle:
                ^{fvInput textView}
                <div> Image Toggle:
                ^{fvInput imageView}
            |]

        rs = [markupRes, textRes, fmap (CT_Toggle_Image . entityKey) imageRes]
        res = fromMaybe (foldr1 (<|>) rs) (listToMaybe $ filter isSuccess rs)
        
    return (res, view)

data ComponentType
    = CT_Markup
    | CT_Toggle_Text SpaceChar
    | CT_Toggle_Image ImageId
    deriving (Show, Read, Eq)

createComponent :: ComponentType -> Handler Component
createComponent CT_Markup = liftM CMarkup createBlankMarkup
createComponent (CT_Toggle_Text sc) = liftM CToggle $ do
    markup <- createBlankMarkup
    return $ ToggleTexts sc [("Blank Toggle", markup)]
createComponent (CT_Toggle_Image bg) = return $
    CToggle $ ToggleImages bg []

createBlankMarkup :: Handler MarkupId
createBlankMarkup = runDB $
    insert $ Markup $ preEscapedToMarkup ("Click to edit..." :: Text)

displayComponent :: SectionId -> Int -> Component -> Widget
displayComponent sectionId cIx (CMarkup markupId) = do
    markup <- handlerToWidget $ runDB $ getJust markupId
    componentId <- newIdent
    contentId <- newIdent
    saveId <- newIdent
    deleteId <- newIdent
    addStylesheet $ StaticR summernote_summernotebs4_css
    addScript $ StaticR summernote_summernotebs4_js
    $(widgetFile "components/markup")
displayComponent sectionId cIx (CToggle t) = [whamlet|Potato|]

deleteComponent :: Component -> Handler ()
deleteComponent (CMarkup markupId) = runDB $ delete markupId
deleteComponent (CToggle t) = case t of
    ToggleTexts _ toggles -> deleteToggles toggles
    ToggleImages _ toggles -> deleteToggles toggles
    where
        deleteToggles :: [ToggleOption a] -> Handler ()
        deleteToggles = mapM_ $ runDB . delete . snd

type CompIx = Int

data ComponentUpdate
    = DeleteComp CompIx
    | UpdateComp CompIx Text
    deriving (Show, Read, Eq, Generic)

instance ToJSON ComponentUpdate where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ComponentUpdate where
    parseJSON = genericParseJSON defaultOptions