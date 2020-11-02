{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Guide where

import Import
import Handler.Component
import Handler.Section
import Handler.Images (getAllImages)
import qualified Data.Text as T (append)
import Text.Blaze (text)
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), renderBootstrap4)

getGuideR :: GuideId -> Handler Html
getGuideR guideId = do
    muser <- maybeAuth
    guide <- runDB $ get404 guideId
    let isAdmin = maybe False (userIsAdmin . entityVal) muser
        published = guideIsPublished guide
    when (not isAdmin && not published) notFound

    --(formWidget, enctype) <- generateFormPost (guideForm $ Just guide)

    defaultLayout $ do
        setTitle $ text $ guideTitle guide
        $(widgetFile "guide")
-- if admin then show edit options

postGuideR :: GuideId -> Handler Html
postGuideR guideId = undefined
    -- user <- requireAuth
    -- guide <- runDB $ get404 guideId
    -- let isAdmin = maybe False (userIsAdmin . entityVal) muser

    -- ((result, formWidget), enctype) <- runFormPost (guideForm $ Just guide)


guideForm :: Maybe Guide -> Form Guide
guideForm mg = renderBootstrap4 BootstrapBasicForm $ Guide
    <$> areq textField "Title" (guideTitle <$> mg)
    <*> areq textField "Url" (guideUrl <$> mg) -- ensure it's valid for a url
    <*> areq checkBoxField "Published" (guideIsPublished <$> mg)
    <*> lift (liftIO getCurrentTime)
    <*> areq (selectField images) "Icon" (guideIcon <$> mg)
    <*> pure (maybe [] guideSections mg)
    where
        images = optionsPersistKey [] [Asc ImageCreated] imageName
        -- improve this to show a preview of the image
        -- also allow searching

deleteGuideR :: GuideId -> Handler ()
deleteGuideR guideId = undefined
    --mGuide <- runDB $ get guideId

getGuideManagerR :: Handler Html
getGuideManagerR = undefined

postGuideManagerR :: Handler Html
postGuideManagerR = undefined