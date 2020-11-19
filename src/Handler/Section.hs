{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Section where

import Import
import Data.Aeson.Types
import Handler.Component
import Handler.Modal
import Handler.Images
import qualified Data.List as L (delete, (!!))
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), renderBootstrap4)

getSectionWidget :: SectionId -> Widget
getSectionWidget sectionId = do
    section <- liftHandler $ runDB $ getJust sectionId
    let guideId = sectionGuideId section
    sForm <- liftHandler $ genBs4FormIdentify
                (sFormIdent $ sectionUrl section)
                (sectionForm guideId $ Just section)

    let sectionModal = mkModal "Edit" sForm
        ncWidget = genNewComponent sectionId
        compWidgets = map (uncurry $ getCompWidget sectionId)
            $ withIndexes $ sectionContent section

    mBackground <- maybe (return Nothing)
                    (liftHandler . runDB . get)
                    (sectionBackground section)

    $(widgetFile "section")

postSectionWidget :: SectionId -> Widget
postSectionWidget sectionId = do
    section <- liftHandler $ runDB $ getJust sectionId
    let guideId = sectionGuideId section
        content = sectionContent section

    ((sRes, sWidget), sEnctype) <- liftHandler $ runBs4FormIdentify
                                        (sFormIdent $ sectionUrl section)
                                        (sectionForm guideId $ Just section)

    let sectionModal = mkModal "Edit" (sWidget, sEnctype)
    
    
    (ncWidget, mcomp) <- liftHandler $ runNewComponent sectionId
    
    liftIO $ putStrLn "c1"
    (compWidgets, mcomps) <- liftHandler $ fmap unzip
        $ mapM (uncurry $ postCompWidget sectionId)
        $ withIndexes content
    liftIO $ putStrLn "c2"

    let onSuccess msg = do
            liftIO $ putStrLn "On success"
            liftHandler $ updateGuideModified $ sectionGuideId section
            setMessage msg
            redirect $ GuideR guideId

    liftIO $ print mcomps
        
    for_ (listToMaybe $ catMaybes mcomps) $ \c@(_, ix) -> do
        liftIO $ putStrLn "Updating component"
        when (ix > 0 || ix >= length content) $ do
            setMessage "Error updating component: index out of bounds"
            redirect $ GuideR guideId
        liftIO $ putStrLn "Running query"
        liftHandler $ runDB $ update sectionId
            [ SectionContent =. content /! c ]
        onSuccess "Component updated"

    for_ mcomp $ \comp -> do
        liftIO $ putStrLn "Adding component to section"
        liftHandler $ runDB $ update sectionId
            [ SectionContent =. content ++ [comp] ]
        onSuccess "Component added"

    case sRes of
        FormSuccess newSection -> do
            liftHandler $ runDB $ update sectionId
                [ SectionTitle =. sectionTitle newSection
                , SectionUrl =. sectionUrl newSection
                , SectionBackground =. sectionBackground newSection
                ]
            onSuccess "Section updated successfully"

        _ -> return ()

    $(widgetFile "section")

data SectionUpdate
    = DeleteComp Int
    deriving (Show, Read, Generic)

instance ToJSON SectionUpdate where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON SectionUpdate where
    parseJSON = genericParseJSON defaultOptions

patchSectionR :: SectionId -> Handler ()
patchSectionR sectionId = do
    mSection <- runDB $ get sectionId
    supdate <- requireCheckJsonBody :: Handler SectionUpdate
    
    for_ mSection $ \section -> do
        let cs = sectionContent section
        case supdate of
            DeleteComp ix
                | ix >= 0 && ix < length cs -> do
                    runDB $ update sectionId $
                        [ SectionContent =. cs -! ix ]
                    updateGuideModified $ sectionGuideId section
                    sendResponse ("Component deleted" :: Text)
                | otherwise -> sendResponseStatus status500
                    ("Index out of bounds " <> tshow ix)

deleteSectionR :: SectionId -> Handler ()
deleteSectionR sectionId = do
    mSection <- runDB $ get sectionId
    case mSection of
        Nothing -> sendResponseStatus status404 ("Section does not exist" :: Text)
        Just section -> do
            let guideId = sectionGuideId section
            updateGuideModified guideId
            removeSectionFromGuide sectionId guideId
            mapM_ deleteComponent $ sectionContent section
            runDB $ delete sectionId
            sendResponse ("Section deleted" :: Text)

sFormIdent :: Text -> Text
sFormIdent url = "section-" <> url

ncFormIdent :: Text -> Text
ncFormIdent url = url <> "-new-comp"

updateGuideModified :: GuideId -> Handler ()
updateGuideModified guideId = liftIO getCurrentTime >>=
    \t -> runDB $ update guideId [GuideModified =. t]

removeSectionFromGuide :: SectionId -> GuideId -> Handler ()
removeSectionFromGuide sectionId guideId = do
    mGuide <- runDB $ get guideId
    case mGuide of
        Nothing -> return ()
        Just guide -> do
            let newSections = L.delete sectionId $ guideSections guide
            runDB $ update guideId [GuideSections =. newSections]

sectionForm :: GuideId -> Maybe Section -> AForm Handler Section
sectionForm guideId msection = Section
    <$> areq textField "Title" (sectionTitle <$> msection)
    <*> areq textField "Url" (sectionUrl <$> msection)
    <*> aopt (selectField images) "Background Image" (sectionBackground <$> msection)
    <*> pure guideId
    <*> pure (maybe [] sectionContent msection)
    where
        images = optionsPersistKey [] [Asc ImageCreated] imageName