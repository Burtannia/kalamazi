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
import qualified Data.List as L (delete)
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), renderBootstrap4)

-- make sure section urls don't contain spaces
getSectionWidget :: SectionId -> Widget
getSectionWidget sectionId = do
    section <- liftHandler $ runDB $ getJust sectionId
    let guideId = sectionGuideId section

    form <- liftHandler $ genBs4FormIdentify
                (sFormIdent $ sectionUrl section)
                (sectionForm guideId $ Just section)

    let modalWidget = mkModal "Edit" form

    $(widgetFile "section")

postSectionWidget :: SectionId -> Widget
postSectionWidget sectionId = do
    section <- liftHandler $ runDB $ getJust sectionId
    let guideId = sectionGuideId section

    ((formRes, formWidget), enctype) <- liftHandler $ runBs4FormIdentify
                                        (sFormIdent $ sectionUrl section)
                                        (sectionForm guideId $ Just section)

    let modalWidget = mkModal "Edit" (formWidget, enctype)

    case formRes of
        FormSuccess newSection -> do
            liftHandler $ runDB $ update sectionId
                [ SectionTitle =. sectionTitle newSection
                , SectionUrl =. sectionUrl newSection
                ]
            setMessage "Section updated successfully"
            redirect $ GuideR $ guideId

        _ -> return ()

    $(widgetFile "section")

sFormIdent :: Text -> Text
sFormIdent url = "section-" <> url

-- belongsTo function is a thing
deleteSectionR :: SectionId -> Handler ()
deleteSectionR sectionId = do
    mSection <- runDB $ get sectionId
    case mSection of
        Nothing -> return ()
        Just section -> do
            removeSectionFromGuide sectionId (sectionGuideId section)
            runDB $ delete sectionId

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
    <*> pure guideId
    <*> pure (maybe [] sectionComponents msection)