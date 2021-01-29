{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Component where

import Import
import Control.Arrow ((&&&))
import Handler.Images
import Handler.Modal
import Summernote
import Data.List (foldr1)
import Data.Maybe (fromJust)
import Data.Bitraversable (bisequence)
import Text.Julius (rawJS)
import Data.Aeson.Types
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), bfs, renderBootstrap4)

import Model.Guide

genNewComponent :: SectionId -> Widget
genNewComponent sectionId = do
    modalId <- newIdent
    let formWidgets = map (uncurry $ genForm modalId) $ withIndexes comps
    $(widgetFile "components/new-component")
    where
        genForm modalId ix (compName, compId', cc) = do
            let isFirst = ix == 0
                compId = modalId <> "-" <> compId'
            formId <- newIdent
            (formWidget, enctype) <- liftHandler
                $ genBs4FormIdentify (mkCreateCompId sectionId compId)
                $ createCompForm cc
            $(widgetFile "components/new-component-form")

runNewComponent :: SectionId -> Handler (Widget, Maybe Component)
runNewComponent sectionId = do
    modalId <- newIdent
    (formWidgets, mcomps) <- fmap unzip $
            mapM (uncurry $ runForm modalId) $ withIndexes comps
    let widget = $(widgetFile "components/new-component")
    return (widget, listToMaybe $ catMaybes mcomps)
    where
        runForm modalId ix (compName, compId', cc) = do
            let isFirst = ix == 0
                compId = modalId <> "-" <> compId'
            formId <- newIdent
            ((formRes, formWidget), enctype) <- liftHandler
                $ runBs4FormIdentify (mkCreateCompId sectionId compId)
                $ createCompForm cc

            mr <- case formRes of
                FormSuccess compData ->
                    fmap Just $ mkComponent compData

                FormMissing -> return Nothing

                FormFailure errs -> do
                    liftIO $ putStrLn "runNewComponent"
                    print errs
                    return Nothing

            let widget = $(widgetFile "components/new-component-form")
            return (widget, mr)

mkCreateCompId :: SectionId -> Text -> Text
mkCreateCompId sectionId t = mkFormId ["create", t, toPathPiece sectionId]

mkEditCompId :: SectionId -> Text -> Text
mkEditCompId sectionId t = mkFormId ["edit", t, toPathPiece sectionId]

-- A form friendly representation of a component to be constructed.
data CreateComponent
    = CreateMarkup (Maybe Html)
    | CreateToggleText (Maybe SpaceChar) [(Text, Html)]
    | CreateToggleImage [(ImageId, Html)]
    | CreateImage (Maybe ImageId)
    | CreateVideo (Maybe Text)
    | CreateWeakAura (Maybe Text) (Maybe Textarea)

comps :: [(Text, Text, CreateComponent)]
comps =
    [ ("Markup", "markup", CreateMarkup Nothing)
    , ("Toggle Texts", "toggletext", CreateToggleText Nothing [])
    , ("Toggle Images", "toggleimage", CreateToggleImage [])
    , ("Image", "image", CreateImage Nothing)
    , ("Video", "video", CreateVideo Nothing)
    , ("WeakAura", "weakaura", CreateWeakAura Nothing Nothing)
    ]

toCreateComp :: Component -> Handler CreateComponent
toCreateComp (CMarkup mId) = CreateMarkup
    <$> fmap (Just . markupBlockContent) (runDB $ getJust mId)
toCreateComp (CToggle (ToggleTexts sc ts)) = CreateToggleText
    <$> pure (Just sc)
    <*> mapM getMarkup ts
toCreateComp (CToggle (ToggleImages ts)) = CreateToggleImage
    <$> mapM getMarkup ts
toCreateComp (CImage imgId) = return $ CreateImage $ Just imgId
toCreateComp (CVideo url) = return $ CreateVideo $ Just url
toCreateComp (CWeakAura title content) = return $ CreateWeakAura (Just title) (Just content)

getMarkup :: (a, MarkupBlockId) -> Handler (a, Html)
getMarkup = sequence . fmap (fmap markupBlockContent . runDB . getJust)

data ComponentData
    = CD_Markup Html
    | CD_ToggleText SpaceChar [(Text, Html)]
    | CD_ToggleImage [(ImageId, Html)]
    | CD_Image ImageId
    | CD_Video Text
    | CD_WeakAura Text Textarea

mkComponent :: ComponentData -> Handler Component
mkComponent (CD_Markup m) = do
    mId <- runDB $ insert $ MarkupBlock m
    return $ CMarkup mId
mkComponent (CD_ToggleText sc ts') = do
    ts <- mapM (traverse (runDB . insert . MarkupBlock)) ts'
    return $ CToggle $ ToggleTexts sc ts
mkComponent (CD_ToggleImage ts') = do
    ts <- mapM (traverse (runDB . insert . MarkupBlock)) ts'
    return $ CToggle $ ToggleImages ts
mkComponent (CD_Image imgId) = return $ CImage imgId
mkComponent (CD_Video url) = return $ CVideo url
mkComponent (CD_WeakAura title content) = return $ CWeakAura title content

createCompForm :: CreateComponent -> AForm Handler ComponentData
createCompForm (CreateMarkup mhtml) = CD_Markup
    <$> areq snFieldUnsanitized (bfs ("Content" :: Text)) mhtml
createCompForm (CreateToggleText msc ts) = CD_ToggleText
    <$> areq (radioFieldList spaceChars) (withClass "mx-1 lg-radio" $ "Space Character") msc
    <*> amulti toggleField groupSettings ts 0 bs4FontistoSettings
    where
        spaceChars = [ ("| - Vertical Bar" :: Text, SpaceLine)
                     , ("> - Chevron", SpaceChev)
                     ]
        toggleField = convertFieldPair
            fst snd (,) textField snFieldUnsanitized "w-100"
        groupSettings = FieldSettings
            { fsLabel = "Groups"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control mb-2")
                , ("placeholder", "Group Label") ]
            }
createCompForm (CreateToggleImage ts) = CD_ToggleImage
    <$> amulti toggleField (bfs ("Groups" :: Text)) ts 0 bs4FontistoSettings
    where
        toggleField = convertFieldPair
            fst snd (,) imageSelectField snFieldUnsanitized "image-group"
createCompForm (CreateImage mimg) = CD_Image
    <$> areq imageSelectField (bfs ("Image" :: Text)) mimg
createCompForm (CreateVideo murl) = CD_Video
    <$> areq textField (withTooltip vidTip $ bfs ("Url" :: Text)) murl
    where
        vidTip = fromString $
            "In order to embed videos from YouTube, the URL must have the following format:"
            <> " https://youtube.com/embed/<video-id>"
createCompForm (CreateWeakAura mtitle mcontent) = CD_WeakAura
    <$> areq textField (withClass "mb-1" $ bfs ("Title" :: Text)) mtitle
    <*> areq textareaField (withClass "minh-12rem" $ bfs ("Content" :: Text)) mcontent

deleteComponent :: Component -> Handler ()
deleteComponent (CMarkup markupId) = runDB $ delete markupId
deleteComponent (CToggle t) = case t of
    ToggleTexts _ toggles -> deleteToggles toggles
    ToggleImages toggles -> deleteToggles toggles
    where
        deleteToggles :: [ToggleOption a] -> Handler ()
        deleteToggles = mapM_ $ runDB . delete . snd
deleteComponent (CImage _) = return ()
deleteComponent (CVideo _) = return ()
deleteComponent (CWeakAura _ _) = return ()

getCompWidget :: Bool -> SectionId -> Int -> Component -> Widget
getCompWidget isAdmin sectionId ix comp = do
    compId <- newIdent
    cc <- liftHandler $ toCreateComp comp

    -- generate edit form
    form <- liftHandler
        $ genBs4FormIdentify (mkEditCompId sectionId $ tshow ix)
        $ createCompForm cc

    -- generate controls and display widget
    let controls = compControls sectionId ix compId form
        compWidget = displayComponent sectionId ix compId comp

    $(widgetFile "component")

postCompWidget :: Bool -> SectionId -> Int -> Component
    -> Handler (Widget, Maybe (Component, Int))
postCompWidget isAdmin sectionId ix comp = do
    compId <- newIdent
    cc <- toCreateComp comp
    ((formRes, formWidget), enctype) <- liftHandler
        $ runBs4FormIdentify (mkEditCompId sectionId $ tshow ix)
        $ createCompForm cc
    let controls = compControls sectionId ix compId (formWidget, enctype)
        compWidget = displayComponent sectionId ix compId comp

    mr <- case formRes of
        FormSuccess compData -> do
            cd <- mkComponent compData
            deleteComponent comp
            return $ Just (cd, ix)

        FormMissing -> return Nothing

        FormFailure errs -> do
            liftIO $ putStrLn "postCompWidget"
            print errs
            return Nothing

    let widget = $(widgetFile "component")
    return (widget, mr)

compControls :: SectionId -> Int -> Text -> (Widget, Enctype) -> Widget
compControls sectionId cIx compId f = do
    delId <- newIdent
    let editWidget = mkModalCustom "Edit Component" f $
            ModalSettings [shamlet| Edit |] "btn btn-primary"
    $(widgetFile "components/controls")

displayComponent :: SectionId -> Int -> Text -> Component -> Widget
displayComponent sectionId cIx compId = displayComponent'
    where
        displayComponent' (CMarkup markupId) = do
            markup <- liftHandler $ runDB $ getJust markupId
            displayMarkup markup
        
        displayComponent' (CToggle (ToggleTexts sc ts')) = do
            ts <- flip mapM ts' $ liftHandler
                                . sequence
                                . fmap (runDB . getJust)
            let headers = map (mkTextSnippet . preEscapedToMarkup . fst) ts
                headerClass = "toggle-text" :: Text
                mSpaceChar = Just $ case sc of
                    SpaceLine -> ("|" :: Text)
                    SpaceChev -> ">"
            toggleId <- newIdent
            $(widgetFile "components/toggle")

        displayComponent' (CToggle (ToggleImages ts')) = do
            ts <- flip mapM ts' $ liftHandler
                                . bisequence
                                . bimap mkImageSnippet (runDB . getJust)
            let headers = map fst ts
                headerClass = "toggle-image" :: Text
                mSpaceChar = Nothing :: Maybe Text
            toggleId <- newIdent
            $(widgetFile "components/toggle")
        
        displayComponent' (CImage imgId) =
            [whamlet|<img src=@{ImagesR $ mkImageUrl imgId}>|]

        displayComponent' (CVideo url) =
            [whamlet|
                <div .video-comp>
                    <div .iframe-wrapper>
                        <iframe src=#{url} frameborder="0" allowfullscreen="true" scrolling="no">
            |]

        displayComponent' (CWeakAura title content) = do
            waId <- newIdent
            $(widgetFile "components/weakaura")

        mkTextSnippet mu = [shamlet| <h6>#{mu} |]

        mkImageSnippet imgId = withUrlRenderer
            [hamlet|<img .img-fluid src=@{ImagesR $ mkImageUrl imgId}>|]

        displayMarkup markup = $(widgetFile "components/markup")