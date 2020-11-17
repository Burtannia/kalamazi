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
import Handler.Component
import Handler.Section
import Handler.Images (getAllImages)
import Handler.Modal
import qualified Data.Text as T (append)
import Text.Blaze (text)
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
        setTitle $ text $ guideTitle guide
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
            runDB $ delete guideId -- why are we doing this unless the url is new?
            newId <- runDB $ insert newGuide -- irritating but necessary to get new id
            let newUrl = guideUrl newGuide
                urlChanged = not $ guideUrl guide == newUrl
            when urlChanged $
                flip mapM_ (guideSections newGuide) $
                    \sId -> runDB $ update sId [SectionUrl =. newUrl]
            -- make sure replacing url works as intended and error is thrown on violated constraint
            setMessage "Guide updated successfully"
            redirect $ GuideR newId
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
        setTitle $ text $ guideTitle guide
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
    <*> areq (selectField images) "Icon" (guideIcon <$> mg)
    <*> pure (maybe [] guideSections mg)
    where
        images = optionsPersistKey [] [Asc ImageCreated] imageName

deleteGuideR :: GuideId -> Handler ()
deleteGuideR guideId = undefined
    --mGuide <- runDB $ get guideId

getGuideManagerR :: Handler Html
getGuideManagerR = undefined

postGuideManagerR :: Handler Html
postGuideManagerR = undefined