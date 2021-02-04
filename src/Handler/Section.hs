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
import qualified Data.Text as T (foldr)
import Text.Julius (rawJS)
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), renderBootstrap4)

getSectionWidget :: Bool -> Guide -> SectionId -> Widget
getSectionWidget isAdmin guide sectionId = do
    section <- liftHandler $ runDB $ getJust sectionId
    let guideId = sectionGuideId section
        secUrl = sectionUrl section
    sForm <- liftHandler $ genBs4FormIdentify
                (sFormIdent secUrl)
                (sectionForm guideId $ Just section)

    let sectionModal = mkModalEdit "Edit" sForm
        ncWidget = genNewComponent sectionId
        compWidgets' = map3 (uncurry $ getCompWidget isAdmin sectionId)
            $ withIndexes3 $ layoutComps $ sectionContent section
        compWidgets = markDivs compWidgets'

    liftIO $ print $ layoutComps $ sectionContent section

    sectionUpId <- newIdent
    sectionDownId <- newIdent
    sectionDelId <- newIdent
    sectionCopyId <- newIdent

    $(widgetFile "section")

postSectionWidget :: Bool -> Guide -> SectionId -> Widget
postSectionWidget isAdmin guide sectionId = do
    section <- liftHandler $ runDB $ getJust sectionId
    let guideId = sectionGuideId section
        secUrl = sectionUrl section
        content = sectionContent section

    ((sRes, sWidget), sEnctype) <- liftHandler $ runBs4FormIdentify
                                        (sFormIdent secUrl)
                                        (sectionForm guideId $ Just section)

    let sectionModal = mkModalEdit "Edit" (sWidget, sEnctype)
    
    (ncWidget, mcomp) <- liftHandler $ runNewComponent sectionId
    
    (compWidgets', mcomps) <- liftHandler
        $ fmap (map3 fst &&& map3 snd)
        $ mapM3 (uncurry $ postCompWidget isAdmin sectionId)
        $ withIndexes3 $ layoutComps content

    let compWidgets = markDivs compWidgets'
        onSuccess msg = do
            liftHandler $ updateGuideModified $ sectionGuideId section
            setMessage msg
            redirect $ GuideR guideId
        
    for_ (listToMaybe $ catMaybes $ concat $ concat mcomps) $ \c@(_, ix) -> do
        when (ix < 0 || ix >= length content) $ do
            setMessage "Error updating component: index out of bounds"
            redirect $ GuideR guideId
        liftHandler $ runDB $ update sectionId
            [ SectionContent =. content /! c ]
        onSuccess "Component updated"

    for_ mcomp $ \comp -> do
        liftHandler $ runDB $ update sectionId
            [ SectionContent =. content ++ [comp] ]
        onSuccess "Component added"

    case sRes of
        FormSuccess newSection -> do
            liftHandler $ runDB $ update sectionId
                [ SectionTitle =. sectionTitle newSection
                , SectionUrl =. sectionUrl newSection
                ]
            onSuccess "Section updated successfully"

        FormMissing -> return ()

        FormFailure errs -> do
            liftIO $ putStrLn "postSectionWidget sRes"
            print errs

    sectionUpId <- newIdent
    sectionDownId <- newIdent
    sectionDelId <- newIdent
    sectionCopyId <- newIdent

    $(widgetFile "section")

-- sectionBgWidget :: Maybe ImageId -> Text -> Widget
-- sectionBgWidget mImgId secUrl =
--     for_ mImgId $ \bg -> toWidget
--         [lucius|
--             ##{secUrl}-wrapper {
--                 background-image: url('@{ImagesR $ mkImageUrl bg}');
--                 background-repeat: no-repeat;
--                 background-position: center;
--                 background-size: auto 70%;
--             }
--         |]

data SectionUpdate
    = DeleteComp Int
    | SectionUp
    | SectionDown
    | CompUp Int
    | CompDown Int
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
        let guideId = sectionGuideId section

        guide <- runDB $ getJust guideId

        let cs = sectionContent section
            allSections = guideSections guide

        case supdate of
            DeleteComp ix
                | ix >= 0 && ix < length cs -> do
                    runDB $ update sectionId $
                        [ SectionContent =. cs -! ix ]
                    updateGuideModified $ sectionGuideId section
                    sendResponse ("Component deleted" :: Text)
                | otherwise -> sendResponseStatus status500
                    ("Index out of bounds " <> tshow ix)
            SectionUp -> do
                let newSections = moveBackward sectionId allSections
                runDB $ update guideId [GuideSections =. newSections]
                updateGuideModified guideId
                sendResponse ("Section moved" :: Text)
            SectionDown -> do
                let newSections = moveForward sectionId allSections
                runDB $ update guideId [GuideSections =. newSections]
                updateGuideModified guideId
                sendResponse ("Section moved" :: Text)
            CompUp ix -> do
                let newComps = moveIxLeft ix cs
                runDB $ update sectionId [SectionContent =. newComps]
            CompDown ix -> do
                let newComps = moveIxRight ix cs
                runDB $ update sectionId [SectionContent =. newComps]
                
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
    <$> areq textField (withPlaceholder "My New Section" $ withClass "mb-1" $ fs "Title" Nothing) (sectionTitle <$> msection)
    <*> areq secUrlField (withPlaceholder "my-new-section" $ fs "Url" urlTip) (sectionUrl <$> msection)
    <*> pure guideId
    <*> pure (maybe [] sectionContent msection)
    where
        urlTip = Just "This will be used to link to the specific section within the guide. Only letters, numbers, hyphens and underscores are permitted."
        bgTip = Just "This will appear faded in the background of the section."
        fs label mtt = FieldSettings
            { fsLabel = label
            , fsTooltip = mtt
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control") ]
            }
        secUrlField = check validateUrl textField
        validateUrl t =
            if isValid
                then Right t
                else Left err
            where
                err :: Text
                err = "Please enter a value containing only letters, numbers, hyphens and underscores)."
                isValid = T.foldr (\c b -> b && c `elem` validChars) True t
                validChars = '-' : '_' : (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])