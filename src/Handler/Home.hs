{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
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
        Just grp -> do
            flip mapM (guideGroupGuides $ entityVal grp) $ \gid -> do
                guide <- runDB $ getJust gid
                return (gid, guideIcon guide, getShortTitle guide)

    defaultLayout $ do
        setTitle "Kalamazi | Warlock Guides for Raid and Mythic+ in World of Warcraft"
        toWidgetHead
            [hamlet| <meta name="description" content=#{pageDescription}> |]
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
        Just grp -> do
            flip mapM (guideGroupGuides $ entityVal grp) $ \gid -> do
                guide <- runDB $ getJust gid
                return (gid, guideIcon guide, getShortTitle guide)

    defaultLayout $ do
        setTitle "Kalamazi | Warlock Guides for Raid and Mythic+ in World of Warcraft"
        toWidgetHead
            [hamlet|<meta name="description" content=#{pageDescription}> |]
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

pageDescription :: Text
pageDescription =
    "The number one source of affliction, destruction and demonology warlock guides for both raid and mythic plus in World of Warcraft Shadowlands."
    <> " Guides include talent builds, best in slot (BiS) gear, ability rotations, stat weights and more..."