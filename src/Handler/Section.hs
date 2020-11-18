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
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), renderBootstrap4)

getSectionWidget :: SectionId -> Widget
getSectionWidget sectionId = do
    section <- liftHandler $ runDB $ getJust sectionId
    let guideId = sectionGuideId section
    sForm <- liftHandler $ genBs4FormIdentify
                (sFormIdent $ sectionUrl section)
                (sectionForm guideId $ Just section)

    ncForm <- liftHandler $ generateFormPost
        $ identifyForm (ncFormIdent $ sectionUrl section) compForm

    imgs <- liftHandler $ runDB getAllImages
    testForm <- liftHandler
        $ genBs4FormIdentify ("fsdfjsaoidjfd")
        $ createCompForm $ CreateToggleImage Nothing []

    let sectionModal = mkModal "Edit" sForm
        newCompModal = mkModal "Add Component" ncForm
        testModal = mkModal "Pepega" testForm

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

    ((ncRes, ncWidget), ncEnctype) <- liftHandler $ runFormPost
            $ identifyForm (ncFormIdent $ sectionUrl section) compForm

    testForm <- liftHandler
        $ genBs4FormIdentify ("fsdfjsaoidjfd")
        $ createCompForm $ CreateMarkup Nothing

    let sectionModal = mkModal "Edit" (sWidget, sEnctype)
        newCompModal = mkModal "Add Component" (ncWidget, ncEnctype)
        testModal = mkModal "Pepega" testForm
        
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

patchSectionR :: SectionId -> Handler ()
patchSectionR sectionId = undefined

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