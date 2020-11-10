{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Section where

import Import
import Handler.Component
import Handler.Modal
import Handler.Images
import qualified Data.List as L (delete, (!!))
import Text.Blaze (preEscapedText)
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), renderBootstrap4)

getSectionWidget :: SectionId -> Widget
getSectionWidget sectionId = do
    section <- liftHandler $ runDB $ getJust sectionId
    let guideId = sectionGuideId section
    sForm <- liftHandler $ genBs4FormIdentify
                (sFormIdent $ sectionUrl section)
                (sectionForm guideId $ Just section)

    ncForm <- liftHandler $ genBs4FormIdentify
                (ncFormIdent $ sectionUrl section)
                compForm

    let sectionModal = mkModal "Edit" sForm
        newCompModal = mkModal "Add Component" ncForm

    mBackground <- maybe (return Nothing)
                    (liftHandler . runDB . get)
                    (sectionBackground section)

    $(widgetFile "section")

postSectionWidget :: SectionId -> Widget
postSectionWidget sectionId = do
    section <- liftHandler $ runDB $ getJust sectionId
    let guideId = sectionGuideId section

    ((sRes, sWidget), sEnctype) <- liftHandler $ runBs4FormIdentify
                                        (sFormIdent $ sectionUrl section)
                                        (sectionForm guideId $ Just section)

    ((ncRes, ncWidget), ncEnctype) <- liftHandler $ runBs4FormIdentify
                                        (ncFormIdent $ sectionUrl section)
                                        compForm

    let sectionModal = mkModal "Edit" (sWidget, sEnctype)
        newCompModal = mkModal "Add Component" (ncWidget, ncEnctype)

    mBackground <- maybe (return Nothing)
                    (liftHandler . runDB . get)
                    (sectionBackground section)

    let onSuccess msg = do
            liftHandler $ updateGuideModified $ sectionGuideId section
            setMessage msg
            redirect $ GuideR guideId

    case sRes of
        FormSuccess newSection -> do
            liftHandler $ runDB $ update sectionId
                [ SectionTitle =. sectionTitle newSection
                , SectionUrl =. sectionUrl newSection
                , SectionBackground =. sectionBackground newSection
                ]
            onSuccess "Section updated successfully"

        _ -> return ()

    case ncRes of
        FormSuccess compType -> do
            comp <- liftHandler $ createComponent compType
            liftHandler $ runDB $ update sectionId
                [SectionContent =. sectionContent section ++ [comp]]
            onSuccess "Component added successfully"
        
        _ -> return ()

    $(widgetFile "section")

sFormIdent :: Text -> Text
sFormIdent url = "section-" <> url

ncFormIdent :: Text -> Text
ncFormIdent url = url <> "-new-comp"

patchSectionR :: SectionId -> Handler ()
patchSectionR sectionId = do
    cUpdate <- requireCheckJsonBody :: Handler ComponentUpdate
    section <- liftHandler $ runDB $ getJust sectionId
    response <- runComponentUpdate (sectionId, section) cUpdate

    case response of
        Left err -> do
            sendResponseStatus status500 err
        Right msg -> do
            updateGuideModified $ sectionGuideId section
            sendResponse msg

runComponentUpdate :: (SectionId, Section) -> ComponentUpdate -> Handler (Either Text Text)
runComponentUpdate (sectionId, section) cUpdate = do
    let oldConts = sectionContent section
    case cUpdate of
        DeleteComp compIx -> boundsCheckM oldConts compIx $ do
            let newConts = oldConts -! compIx
            deleteComponent $ oldConts L.!! compIx
            runDB $ update sectionId [SectionContent =. newConts]
            return "COMPONENT DELETED"
        UpdateComp compIx t -> boundsCheckM oldConts compIx $ do
            let (CMarkup mId) = oldConts L.!! compIx
            runDB $ update mId [MarkupContent =. preEscapedText t]
            return "COMPONENT UPDATED"

deleteSectionR :: SectionId -> Handler ()
deleteSectionR sectionId = do
    mSection <- runDB $ get sectionId
    case mSection of
        Nothing -> return ()
        Just section -> do
            let guideId = sectionGuideId section
            updateGuideModified guideId
            removeSectionFromGuide sectionId guideId
            mapM_ deleteComponent $ sectionContent section
            runDB $ delete sectionId
            sendResponse ("SECTION DELETED" :: Text)

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