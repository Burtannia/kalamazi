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
import Data.Bitraversable (bisequence)
import Text.Julius (rawJS)
import Data.Aeson.Types
import Text.Blaze (preEscapedText)
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), bfs, renderBootstrap4)

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
        imageField = imageSelectField

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
displayComponent sectionId cIx comp = do
    compId <- newIdent
    $(widgetFile "component")
    where
        displayComponent' compId (CMarkup markupId) = do
            markup <- liftHandler $ runDB $ getJust markupId
            displayMarkup compId markup
        
        displayComponent' compId (CToggle (ToggleTexts sc ts')) = do
            ts <- flip mapM ts' $ liftHandler
                                . sequence
                                . fmap (runDB . getJust)
            let headers = map (preEscapedText . fst) ts
            $(widgetFile "components/toggle")

        displayComponent' compId (CToggle (ToggleImages bg ts')) = do
            ts <- flip mapM ts' $ liftHandler
                                . bisequence
                                . bimap mkImageSnippet (runDB . getJust)
            let headers = map fst ts
            $(widgetFile "components/toggle")
        
        mkImageSnippet imgId = do
            img <- runDB $ getJust imgId
            withUrlRenderer
                [hamlet|<img src=@{ImagesR $ mkImageUrl img}>|]

displayMarkup :: Text -> Markup -> Widget
displayMarkup compId markup = do
    addStylesheet $ StaticR summernote_summernotebs4_css
    addScript $ StaticR summernote_summernotebs4_js
    contentId <- newIdent
    saveId <- newIdent
    deleteId <- newIdent
    $(widgetFile "components/markup")

--runCreateComponent :: CreateComponent -> Component

data CCS = CCS
    { ccsSave :: Bool
    , ccsDelete :: Bool
    -- some sort of edit widget
    }

-- TODO:
-- consider how to use createCompForm to make new components
-- not just edit existing ones
-- maybe present the different forms for each possibility
-- then switch between them with JS, use input form if necessary

-- componentControls :: CCS -> Widget

createCompForm :: CreateComponent -> AForm Handler CreateComponent
createCompForm (CreateMarkup m) = CreateMarkup
    <$> areq snFieldUnsanitized "Content" (Just m)
createCompForm (CreateToggleText sc ts) = CreateToggleText
    <$> areq (radioFieldList spaceChars) "Space Character" (Just sc)
    <*> amulti toggleField (bfs ("Sections" :: Text)) ts 0 bs4FASettings
    where
        spaceChars = [ ("Vertical Line |" :: Text, SpaceLine)
                     , ("Chevron >", SpaceChev)
                     ]
        toggleField = convertFieldPair
            fst snd (,) textField snFieldUnsanitized
createCompForm (CreateToggleImage brd ts) = CreateToggleImage
    <$> areq imageSelectField "Border Image" (Just brd)
    <*> amulti toggleField (bfs ("Sections" :: Text)) ts 0 bs4FASettings
    where
        toggleField = convertFieldPair
            fst snd (,) imageSelectField snFieldUnsanitized

convertFieldPair :: (c -> a)
    -> (c -> b)
    -> (a -> b -> c)
    -> Field Handler a
    -> Field Handler b
    -> Field Handler c
convertFieldPair toA toB toC fa fb = Field
    { fieldParse = \rawVals fileVals -> do
        let parseA = fieldParse fa
            parseB = fieldParse fb

        eResA <- parseA rawVals fileVals
        eResB <- parseB (safeTail rawVals) (safeTail fileVals)

        return $ liftA2 (liftA2 toC) eResA eResB

    , fieldView = \ti tn as eRes req -> do
        let viewA = fieldView fa
            viewB = fieldView fb
        [whamlet|
            <div ##{ti}>
                ^{viewA (ti <> "-A") tn as (fmap toA eRes) req}
                ^{viewB (ti <> "-B") tn as (fmap toB eRes) req}
        |]
    , fieldEnctype = fieldEnctype fa
    }

deleteComponent :: Component -> Handler ()
deleteComponent (CMarkup markupId) = runDB $ delete markupId
deleteComponent (CToggle t) = case t of
    ToggleTexts _ toggles -> deleteToggles toggles
    ToggleImages _ toggles -> deleteToggles toggles
    where
        deleteToggles :: [ToggleOption a] -> Handler ()
        deleteToggles = mapM_ $ runDB . delete . snd

updateComponent :: Component -> Text -> Handler ()
updateComponent (CMarkup markupId) txt =
    runDB $ update markupId [MarkupContent =. preEscapedText txt]
updateComponent (CToggle t) txt = undefined

type CompIx = Int

data ComponentUpdate
    = DeleteComp CompIx
    | UpdateComp CompIx Text
    deriving (Show, Read, Eq, Generic)

instance ToJSON ComponentUpdate where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ComponentUpdate where
    parseJSON = genericParseJSON defaultOptions