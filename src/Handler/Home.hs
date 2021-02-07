{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Home where

import Import
import Handler.Images
import Handler.Guide
import Handler.AdminTools
import Handler.YouTube
       
getHomeR :: Handler Html
getHomeR = do
    muser <- maybeAuth
    let isAdmin = maybe False (userIsAdmin . entityVal) muser

    imgs <- runDB getAllImages
    ytVideo <- getLatestVideo

    mGroup <- runDB $ getBy $ UniqueGuideGroupName homeGroupName
    guides <- case mGroup of
        Nothing -> return []
        Just group -> do
            flip mapM (guideGroupGuides $ entityVal group) $ \gid -> do
                guide <- runDB $ getJust gid
                return (gid, guideIcon guide)

    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        let madminTools =
                if isAdmin then
                    Just $ mkAdminTools $ 
                        AdminTools
                        getImageManager
                        getGroupManager
                        (genNewGuide imgs)
                else
                    Nothing
        $(widgetFile "homepage")        

postHomeR :: Handler Html
postHomeR = do
    muser <- maybeAuth
    let isAdmin = maybe False (userIsAdmin . entityVal) muser

    imgs <- runDB getAllImages
    ytVideo <- getLatestVideo

    mGroup <- runDB $ getBy $ UniqueGuideGroupName homeGroupName
    guides <- case mGroup of
        Nothing -> return []
        Just group -> do
            flip mapM (guideGroupGuides $ entityVal group) $ \gid -> do
                guide <- runDB $ getJust gid
                return (gid, guideIcon guide)

    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        let madminTools =
                if isAdmin then
                    Just $ mkAdminTools $ 
                        AdminTools
                        postImageManager
                        postGroupManager
                        (runNewGuide imgs)
                else
                    Nothing
        $(widgetFile "homepage")

