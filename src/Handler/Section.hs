{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Section where

import Import
import Handler.Component
import qualified Data.List as L (delete)

-- make sure section urls don't contain spaces
displaySection :: SectionId -> Widget
displaySection sectionId = do
    section <- handlerToWidget $ runDB $ getJust sectionId
    [whamlet|
        <h2 ##{sectionUrl section}>#{sectionTitle section}
        $forall comp <- sectionComponents section
            ^{displayComponent comp}
    |]
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