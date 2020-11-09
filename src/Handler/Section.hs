{-# LANGUAGE DeriveGeneric #-}
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
import Data.Aeson.Types
import Text.Blaze (preEscapedText)
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), renderBootstrap4)

-- make sure section urls don't contain spaces
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

    case sRes of
        FormSuccess newSection -> do
            liftHandler $ runDB $ update sectionId
                [ SectionTitle =. sectionTitle newSection
                , SectionUrl =. sectionUrl newSection
                , SectionBackground =. sectionBackground newSection
                ]
            setMessage "Section updated successfully"
            redirect $ GuideR guideId

        _ -> return ()

    case ncRes of
        FormSuccess compType -> do
            comp <- liftHandler $ createComponent compType
            liftHandler $ runDB $ update sectionId
                [SectionContent =. sectionContent section ++ [comp]]
            setMessage "Component created successfully"
            redirect $ GuideR guideId
        
        _ -> return ()

    $(widgetFile "section")

sFormIdent :: Text -> Text
sFormIdent url = "section-" <> url

ncFormIdent :: Text -> Text
ncFormIdent url = url <> "-new-comp"

patchSectionR :: SectionId -> Handler ()
patchSectionR sectionId = do
    change <- requireCheckJsonBody :: Handler SectionUpdate
    section <- liftHandler $ runDB $ getJust sectionId
    let oldConts = sectionContent section
    return ()
    -- case change of
    --     DelComp compIx -> do
    --         let newConts = oldConts -! compIx
    --         runDB $ update sectionId [SectionContent =. newConts]
    --         sendResponse ("COMPONENT DELETED" :: Text)
    --     UpdateComp compIx t -> do
    --         boundsCheckM_ oldConts compIx $
    --             \n xs -> runDB $ update (xs L.!! n) [MarkupContent =. preEscapedText t]
    --         sendResponse ("COMPONENT UPDATED" :: Text)

-- belongsTo function is a thing
deleteSectionR :: SectionId -> Handler ()
deleteSectionR sectionId = do
    mSection <- runDB $ get sectionId
    case mSection of
        Nothing -> return ()
        Just section -> do
            removeSectionFromGuide sectionId (sectionGuideId section)
            --mapM_ (runDB . delete) $ sectionContent section
            runDB $ delete sectionId
            sendResponse ("SECTION DELETED" :: Text)

-- deleteComp :: Component -> Handler ()
-- deleteComp (CMarkup markupId) = runDB $ delete markupId

-- deleteComp (CToggle t) = case t of
--     ToggleTexts _ toggles -> deleteToggles toggles
--     ToggleImages _ toggles -> deleteToggles toggles
--     where
--         deleteToggles :: [ToggleOption a] -> Handler ()
--         deleteToggles = mapM_ $ runDB . delete . snd

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

type CompIx = Int

data SectionUpdate
    = DelComp CompIx
    | UpdateComp CompIx Text
    deriving (Show, Read, Eq, Generic)

instance ToJSON SectionUpdate where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON SectionUpdate where
    parseJSON = genericParseJSON defaultOptions