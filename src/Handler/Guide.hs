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
import Data.Time.Format.ISO8601
import Handler.AdminTools
import Handler.Component
import Handler.Section
import Handler.Images
import Handler.Modal
import qualified Data.Text as T (foldr)
import Data.Time.Clock (diffUTCTime)
import Yesod.Form.Bootstrap4 (bfs)

getGuideR :: GuideId -> Handler Html
getGuideR guideId = do
    muser <- maybeAuth
    guide <- runDB $ get404 guideId
    let isAdmin = maybe False (userIsAdmin . entityVal) muser
        published = guideIsPublished guide
    when (not isAdmin && not published) notFound

    imgs <- runDB getAllImages

    -- Guide Form
    gForm <- genBs4FormIdentify gFormIdent $ guideForm imgs $ Just guide
    let guideEditWidget = mkModalEdit "Edit" gForm

    -- Sections    
    let sectionWidgets = map (getSectionWidget imgs isAdmin) $ guideSections guide

    nsForm <- genBs4FormIdentify nsFormIdent $ sectionForm guideId Nothing
    let nsWidget = mkModalAdd "New Section" nsForm

    timeNow <- liftIO getCurrentTime

    defaultLayout $ do
        setTitle $ toHtml $ guideTitle guide
        guideMeta guideId guide
        let madminTools =
                if isAdmin then
                    Just $ mkAdminTools $ 
                        AdminTools
                        getImageManager
                        getGroupManager
                        (genNewGuide imgs)
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

    imgs <- runDB getAllImages

    -- Guide Form
    ((gResult, gWidget), gEnctype) <- runBs4FormIdentify gFormIdent
                                        $ guideForm imgs $ Just guide

    let guideEditWidget = mkModalEdit "Edit" (gWidget, gEnctype)

    case gResult of
        FormSuccess newGuide -> do
            let newUrl = guideUrl newGuide
                urlChanged = not $ guideUrl guide == newUrl
            theId <-
                if urlChanged then do
                    newId <- runDB $ insert400 newGuide
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
        sectionWidgets = map (postSectionWidget imgs isAdmin) sections

    ((nsResult, nsWidget'), nsEnctype) <- runBs4FormIdentify nsFormIdent
                                            $ sectionForm guideId Nothing
    let nsWidget = mkModalAdd "New Section" (nsWidget', nsEnctype)

    -- New Section
    case nsResult of
        FormSuccess newSection -> do
            newSectionId <- runDB $ insert400 newSection
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
        guideMeta guideId guide
        let madminTools =
                if isAdmin then
                    Just $ mkAdminTools $ 
                        AdminTools
                        postImageManager
                        postGroupManager
                        (runNewGuide imgs)
                else
                    Nothing
            timeAgo = diffUTCTime timeNow $ guideModified guide
        $(widgetFile "guide")

guideMeta :: GuideId -> Guide -> Widget
guideMeta guideId guide = toWidgetHead
    [hamlet|
        <meta name=description content=#{guideDescription guide}>
        <script type="application/ld+json">
            {
                "@context": "https://schema.org",
                "@type": "Article",
                "mainEntityOfPage": {
                    "@type": "WebPage",
                    "@id": @{GuideR guideId}
                },
                "headline": #{guideDescription guide},
                "image": [@{ImagesR $ mkImageUrl $ guideIcon guide}],
                "dateModified": #{iso8601Show $ guideModified guide},
                "author": {
                    "@type": "Person",
                    "name": "Kalamazi",
                    "url": "https://www.kalamazi.gg"
                }
            }
    |]

gFormIdent :: Text
gFormIdent = "guide"

nsFormIdent :: Text
nsFormIdent = "new-section"

guideForm :: [Entity Image] -> Maybe Guide -> AForm Handler Guide
guideForm imgs mg = Guide
    <$> areq textField titleSettings (guideTitle <$> mg)
    <*> aopt textField (withPlaceholder "My New Guide" $ fSettings "Short Title (Optional)" $ Just shortTip) (guideShortTitle <$> mg) 
    <*> areq textField (withPlaceholder "Describe the guide..." $ fSettings "Description" $ Just descTip) (guideDescription <$> mg) 
    <*> areq gUrlField (withPlaceholder "my-new-guide" $ fSettings "Url" $ Just urlTip) (guideUrl <$> mg)
    <*> areq checkBoxField pubSettings (guideIsPublished <$> mg)
    <*> lift (liftIO getCurrentTime)
    <*> areq (imageSelectField imgs) (fSettings "Icon" $ Just iconTip) (guideIcon <$> mg)
    <*> pure (maybe [] guideSections mg)
    where
        gUrlField = check validateUrl textField
        validateUrl t =
            if isValid
                then Right t
                else Left err
            where
                err :: Text
                err = "Please enter a value containing only letters, numbers, hyphens and underscores)."
                isValid = T.foldr (\c b -> b && c `elem` validChars) True t
                validChars = '-' : '_' : (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
        titleSettings = FieldSettings
            { fsLabel = "Guide Title"
            , fsTooltip = Just "This will also be used as the title of the webpage."
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control mb-1")
                , ("placeholder", "My New Guide For Things and Stuff and Things") ]
            }
        fSettings label mtt = FieldSettings
            { fsLabel = label
            , fsTooltip = mtt
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control") ]
            }
        pubSettings = FieldSettings
            { fsLabel = "Published"
            , fsTooltip = Just "Guides that are not published will only be viewable by admins."
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "lg-checkbox") ]
            }
        shortTip = "Optional short title to be displayed in links on the homepage / navbar etc."
        descTip = "This will be used as the description for search engines like Google."
        urlTip = "The guide will be at kalamazi.gg/guides/<url>. Only letters, numbers, hyphens and underscores are permitted."
        iconTip = "This image will be used as a thumbnail if the guide is displayed on the homepage."

getShortTitle :: Guide -> Text
getShortTitle g = fromMaybe (guideTitle g) $ guideShortTitle g

genNewGuide :: [Entity Image] -> Widget
genNewGuide imgs = do
    form <- liftHandler $ genBs4FormIdentify ngFormIdent $ guideForm imgs Nothing
    mkModal "New Guide" form

runNewGuide :: [Entity Image] -> Widget
runNewGuide imgs = do
    ((result, formWidget), enctype) <- liftHandler $
        runBs4FormIdentify ngFormIdent $ guideForm imgs Nothing
    case result of
        FormSuccess guide -> do
            guideId <- liftHandler $ runDB $ insert400 guide
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

guideGroupForm :: AForm Handler GuideGroup
guideGroupForm = GuideGroup
    <$> areq textField groupNameSettings Nothing
    <*> pure 0 -- not used
    <*> amulti (selectField guideOptions) (bfs ("Guides" :: Text)) [] 1 bs4LISettings
    where
        guideOptions = optionsPersistKey [] [Asc GuideTitle] getShortTitle
        groupNameSettings = FieldSettings
            { fsLabel = "Group Name"
            , fsTooltip = Just $ fromString $
                            "This name will show as the link text when on the navbar."
                            ++ " Guides in the group \"" ++ unpack homeGroupName ++ "\" will be"
                            ++ " displayed as images on the homepage and not linked on the"
                            ++ " navbar. Creating a group with a name that is already in"
                            ++ " use will replace the existing group."
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "My Group Name") ]
            }

getGroupManager :: Widget
getGroupManager = do
    (ggFormWidget, ggEnctype) <- liftHandler $ genBs4FormIdentify ggFormIdent $ guideGroupForm

    ggs <- liftHandler ggHelper

    modalId <- newIdent
    $(widgetFile "guide-groups")

postGroupManager :: Widget
postGroupManager = do
    ((ggRes, ggFormWidget), ggEnctype) <- liftHandler $ runBs4FormIdentify ggFormIdent $ guideGroupForm
    liftHandler $ liftIO $ print ggRes
    case ggRes of
        FormSuccess gg' -> do
            let gg = gg' { guideGroupName = trimWhitespace $ guideGroupName gg' }
            mg <- liftHandler $ runDB $ getBy $ UniqueGuideGroupName $ guideGroupName gg
            numGroups <- liftHandler $ runDB $ count ([] :: [Filter GuideGroup])
            case mg of
                Nothing -> do
                    _ <- liftHandler $ runDB $
                        insert400 (gg {guideGroupPosition = numGroups + 1})
                    return ()
                Just entGG -> do
                    _ <- liftHandler $ runDB $
                        replace (entityKey entGG) (gg {guideGroupPosition = guideGroupPosition $ entityVal entGG})
                    return ()
            mcr <- getCurrentRoute
            for_ mcr $ \cr -> redirect cr

        FormMissing -> return ()

        FormFailure errs -> do
            liftIO $ putStrLn "postGroupManager ggRes"
            print errs

    ggs <- liftHandler ggHelper

    modalId <- newIdent
    $(widgetFile "guide-groups")

ggHelper :: Handler [(GuideGroup, Map (Key Guide) Guide)]
ggHelper = do
    ggs' <- fmap (map entityVal) $
        runDB $ selectList [] [Asc GuideGroupPosition]

    mapM (sequence . (id &&& runDB . getMany . guideGroupGuides)) ggs'

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