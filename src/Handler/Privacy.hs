{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Privacy where

import Import

getPrivacyR :: Handler Html
getPrivacyR = do
    muser <- maybeAuth
    let loggedIn = isJust muser
    defaultLayout $ do
        setTitle "Privacy - GDPR"
        $(widgetFile "privacy")

deletePrivacyR :: Handler ()
deletePrivacyR = do
    muser <- maybeAuth
    for_ muser $ \euser -> do
        runDB $ delete $ entityKey euser
        setMessage "Details removed successfully"
        sendResponse ("Details removed successfully" :: Text)
    sendResponseStatus status500 ("You are not logged in" :: Text)