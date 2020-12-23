{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Guide where

import Import
import Data.Aeson.Types
import Handler.AdminTools
import Handler.Component
import Handler.Section
import Handler.Images
import Handler.Modal
import qualified Data.Text as T (append)
import Data.Time.Clock (diffUTCTime)
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), renderBootstrap4)

getGuideR :: GuideId -> Handler Html
getGuideR guideId = do
    muser <- maybeAuth
    guide <- runDB $ get404 guideId
    let isAdmin = maybe False (userIsAdmin . entityVal) muser
        published = guideIsPublished guide
    when (not isAdmin && not published) notFound

    -- Guide Form
    gForm <- genBs4FormIdentify gFormIdent $ guideForm $ Just guide
    let gWidget =
            [whamlet|
                ^{mkModal "Edit" gForm}
                <button #delete-guide .btn .btn-danger type="button">Delete
            |] 

    -- Sections    
    let sectionWidgets = map (getSectionWidget isAdmin) $ guideSections guide

    
    nsForm <- genBs4FormIdentify nsFormIdent $ sectionForm guideId Nothing
    let nsWidget = mkModal "New Section" nsForm

    timeNow <- liftIO getCurrentTime

    defaultLayout $ do
        setTitle $ toHtml $ guideTitle guide
        let madminTools =
                if isAdmin then
                    Just $ mkAdminTools $ 
                        AdminTools
                        getImageManager
                        ggManager
                        genNewGuide
                        (Just gWidget)
                else
                    Nothing
            timeAgo = diffUTCTime timeNow $ guideModified guide
        $(widgetFile "guide")

postGuideR :: GuideId -> Handler Html
postGuideR guideId = do
    muser <- maybeAuth
    guide <- runDB $ get404 guideId
    let isAdmin = maybe False (userIsAdmin . entityVal) muser
        published = guideIsPublished guide
    when (not isAdmin && not published) notFound

    -- Guide Form
    ((gResult, gWidget'), gEnctype) <- runBs4FormIdentify gFormIdent
                                        $ guideForm $ Just guide
    let gWidget =
            [whamlet|
                ^{mkModal "Edit" (gWidget', gEnctype)}
                <button #delete-guide .btn .btn-danger type="button">Delete
            |]

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

        FormMissing -> return ()

        FormFailure errs -> do
            liftIO $ putStrLn "runNewComponent"
            print errs

    -- Sections
    let sections = guideSections guide
        sectionWidgets = map (postSectionWidget isAdmin) sections

    ((nsResult, nsWidget'), nsEnctype) <- runBs4FormIdentify nsFormIdent
                                            $ sectionForm guideId Nothing
    let nsWidget = mkModal "New Section" (nsWidget', nsEnctype)

    -- New Section
    case nsResult of
        FormSuccess newSection -> do
            newSectionId <- runDB $ insert newSection
            runDB $ update guideId [GuideSections =. sections ++ [newSectionId]]
            updateGuideModified guideId
            setMessage "Section created successfully"
            redirect $ GuideR guideId

        FormMissing -> return ()

        FormFailure errs -> do
            liftIO $ putStrLn "postGuideR nsResult"
            print errs

    timeNow <- liftIO getCurrentTime

    defaultLayout $ do
        setTitle $ toHtml $ guideTitle guide
        let madminTools =
                if isAdmin then
                    Just $ mkAdminTools $ 
                        AdminTools
                        postImageManager
                        ggManager
                        runNewGuide
                        (Just gWidget)
                else
                    Nothing
            timeAgo = diffUTCTime timeNow $ guideModified guide
        $(widgetFile "guide")

gFormIdent :: Text
gFormIdent = "guide"

nsFormIdent :: Text
nsFormIdent = "new-section"

guideForm :: Maybe Guide -> AForm Handler Guide
guideForm mg = Guide
    <$> areq textField "Title" (guideTitle <$> mg)
    <*> fmap remSpecialChars (areq textField "Url" (guideUrl <$> mg))
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

        FormFailure errs -> do
            liftIO $ putStrLn "runNewGUide"
            print errs

    mkModal "New Guide" (formWidget, enctype)

ngFormIdent :: Text
ngFormIdent = "new-guide"

deleteGuideR :: GuideId -> Handler ()
deleteGuideR guideId = do
    mGuide <- runDB $ get guideId
    for_ mGuide $ \guide -> do
        sections <- fmap catMaybes $ mapM (runDB . get) $ guideSections guide
        mapM_ deleteComponent $ concatMap sectionContent sections
        mapM_ (runDB . delete) $ guideSections guide
        runDB $ delete guideId
        removeFromGroups guideId
        setMessage "Guide deleted successfully"
        sendResponse ("Guide deleted successfully" :: Text)
    sendResponseStatus status404 ("Guide does not exist" :: Text)

removeFromGroups :: GuideId -> Handler ()
removeFromGroups guideId = do
    ggs <- runDB $ selectList [] [Asc GuideGroupPosition]
    flip mapM_ ggs $ \entgg -> do
        let ggGuides = guideGroupGuides $ entityVal entgg
        when (guideId `elem` ggGuides) $
            runDB $ update (entityKey entgg) [GuideGroupGuides =. ggGuides -=! guideId]

ggManager :: Widget
ggManager = do
    ggs' <- fmap (map entityVal) $
        liftHandler $ runDB $ selectList [] [Asc GuideGroupPosition]
    guides' <- liftHandler $ runDB $ selectList [] [Asc GuideTitle]
    let guides = map entityVal guides'
    ggs <- liftHandler $
        mapM (sequence . (id &&& runDB . getMany . guideGroupGuides)) ggs'
    modalId <- newIdent
    $(widgetFile "guide-groups")

postGroupManagerR :: Handler Value
postGroupManagerR = do
    gg' <- requireCheckJsonBody :: Handler GuideGroup
    numGroups <- runDB $ count ([] :: [Filter GuideGroup])
    gg <- runDB $ insertEntity (gg' {guideGroupPosition = numGroups + 1})
    returnJson gg

patchGuideGroupR :: GuideGroupId -> Handler ()
patchGuideGroupR ggid = do
    gg <- runDB $ getJust ggid
    change <- requireCheckJsonBody :: Handler GGUpdate
    numGroups <- runDB $ count ([] :: [Filter GuideGroup])
    let pos = guideGroupPosition gg
    case change of
        GGUp
            | pos == 1 -> return ()
            | otherwise -> do
                runDB $ updateWhere [GuideGroupPosition ==. pos - 1] [GuideGroupPosition +=. 1]
                runDB $ update ggid [GuideGroupPosition -=. 1]

        GGDown
            | pos == numGroups -> return ()
            | otherwise -> do
                runDB $ updateWhere [GuideGroupPosition ==. pos + 1] [GuideGroupPosition -=. 1]
                runDB $ update ggid [GuideGroupPosition +=. 1]

data GGUpdate = GGUp | GGDown
    deriving (Show, Read, Generic)

instance ToJSON GGUpdate where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON GGUpdate where
    parseJSON = genericParseJSON defaultOptions

deleteGuideGroupR :: GuideGroupId -> Handler ()
deleteGuideGroupR ggid = do
    mgg <- runDB $ get ggid
    for_ mgg $ \gg -> do
        let pos = guideGroupPosition gg
        runDB $ delete ggid
        runDB $ updateWhere [GuideGroupPosition >. pos] [GuideGroupPosition -=. 1]
        sendResponse ("Group deleted" :: Text)
    sendResponseStatus status404 ("Group does not exist" :: Text)

ggFormIdent :: Text
ggFormIdent = "guide-groups"