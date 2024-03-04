{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.ComponentEdit where
    
import Import
import Handler.Component
import Handler.Images
import qualified Data.List as L ((!!))

getComponentEditR :: SectionId -> Int -> Text -> Handler Html
getComponentEditR secId compIx compFormId = do
    section <- runDB $ getJust secId
    imgs <- runDB getAllImages

    let comp = sectionContent section L.!! compIx
    cc <- toCreateComp comp

    -- generate edit form
    (formWidget, enctype) <- genBs4FormIdentify compFormId
        $ createCompForm imgs cc

    formContent <- widgetToPageContent formWidget

    withUrlRenderer [hamlet|
        <form ##{compFormId} method=post enctype=#{enctype}>
            ^{pageBody formContent}
    |]
