{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Handler.Guide where

import Import
import Handler.AdminTools
import Handler.Component
import Handler.Section
import Handler.Images
import Handler.Modal
import qualified Data.Text as T (append)
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), renderBootstrap4)

getGuideR :: GuideId -> Handler Html
getGuideR guideId = do
    muser <- maybeAuth
    guide <- runDB $ get404 guideId
    let isAdmin = maybe False (userIsAdmin . entityVal) muser
        published = guideIsPublished guide
    --when (not isAdmin && not published) notFound

    -- Guide Form
    gForm <- genBs4FormIdentify gFormIdent $ guideForm $ Just guide
    let gWidget = mkModal "Edit" gForm

    -- Sections    
    let sectionWidgets = map getSectionWidget $ guideSections guide

    -- New Section
    nsForm <- genBs4FormIdentify nsFormIdent $ sectionForm guideId Nothing
    let nsWidget = mkModal "New Section" nsForm

    defaultLayout $ do
        setTitle $ toHtml $ guideTitle guide
        let madminTools = Just $ mkAdminTools $ AdminTools
                                                getImageManager
                                                genNewGuide
                                                Nothing
        $(widgetFile "guide")
    -- if admin then show edit options

postGuideR :: GuideId -> Handler Html
postGuideR guideId = do
    muser <- maybeAuth
    guide <- runDB $ get404 guideId
    let isAdmin = maybe False (userIsAdmin . entityVal) muser
        published = guideIsPublished guide
    --when (not isAdmin && not published) notFound

    -- Guide Form
    ((gResult, gWidget'), gEnctype) <- runBs4FormIdentify gFormIdent
                                        $ guideForm $ Just guide
    let gWidget = mkModal "Edit" (gWidget', gEnctype)

    case gResult of
        FormSuccess newGuide -> do
            let newUrl = guideUrl newGuide
                urlChanged = not $ guideUrl guide == newUrl

            theId <-
                if urlChanged then do
                    newId <- runDB $ insert newGuide
                    runDB $ delete guideId
                    flip mapM_ (guideSections newGuide) $
                        \sId -> runDB $ update sId [SectionGuideId =. newId]
                    return newId
                else do
                    runDB $ replace guideId newGuide
                    return guideId

            setMessage "Guide updated successfully"
            redirect $ GuideR theId
        _ -> return ()

    -- Sections
    let sections = guideSections guide
        sectionWidgets = map postSectionWidget sections

    -- New Section
    ((nsResult, nsWidget), nsEnctype) <- runBs4FormIdentify nsFormIdent
                                            $ sectionForm guideId Nothing
    let nsWidget = mkModal "New Section" (nsWidget, nsEnctype)

    case nsResult of
        FormSuccess newSection -> do
            newSectionId <- runDB $ insert newSection
            runDB $ update guideId [GuideSections =. sections ++ [newSectionId]]
            updateGuideModified guideId
            setMessage "Section created successfully"
            redirect $ GuideR guideId
        _ -> return ()

    defaultLayout $ do
        setTitle $ toHtml $ guideTitle guide
        let madminTools = Just $ mkAdminTools $ AdminTools
                                                postImageManager
                                                runNewGuide
                                                Nothing
        $(widgetFile "guide")

gFormIdent :: Text
gFormIdent = "guide"

nsFormIdent :: Text
nsFormIdent = "new-section"

guideForm :: Maybe Guide -> AForm Handler Guide
guideForm mg = Guide
    <$> areq textField "Title" (guideTitle <$> mg)
    <*> areq textField "Url" (guideUrl <$> mg) -- ensure it's valid for a url
    <*> areq checkBoxField "Published" (guideIsPublished <$> mg)
    <*> lift (liftIO getCurrentTime)
    <*> areq imageSelectField "Icon" (guideIcon <$> mg)
    <*> pure (maybe [] guideSections mg)

genNewGuide :: Widget
genNewGuide = do
    form <- liftHandler $ genBs4FormIdentify ngFormIdent $ guideForm Nothing
    mkModal "New Guide" form

runNewGuide :: Widget
runNewGuide = do
    ((result, formWidget), enctype) <- liftHandler $
        runBs4FormIdentify ngFormIdent $ guideForm Nothing

    case result of
        FormSuccess guide -> do
            guideId <- liftHandler $ runDB $ insert guide
            liftHandler $ setMessage "Guide created successfully"
            redirect $ GuideR guideId

        FormMissing -> return ()

        _ -> liftHandler $ msgRedirect "Something went wrong"

    mkModal "New Guide" (formWidget, enctype)

ngFormIdent :: Text
ngFormIdent = "new-guide"

deleteGuideR :: GuideId -> Handler ()
deleteGuideR guideId = undefined
    --mGuide <- runDB $ get guideId

getGuideManagerR :: Handler Html
getGuideManagerR = undefined

postGuideManagerR :: Handler Html
postGuideManagerR = undefined