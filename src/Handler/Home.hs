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
        setTitle homeTitle
        pageMeta
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
        setTitle homeTitle
        pageMeta
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

homeTitle :: Html
homeTitle = "Kalamazi | Warlock Guides for World of Warcraft"

pageMeta :: Widget
pageMeta = do
    setLogoMetaImage
    toWidgetHead [hamlet|
        <meta name="description" content=#{pageDescription}>
        <script type="application/ld+json">
            {
                "@context": "https://schema.org",
                "@type": "Person",
                "url": "https://www.kalamazi.gg",
                "image": "@{StaticR logo_full_png}",
                "name": "Kalamazi"
            }
    |]

pageDescription :: Text
pageDescription =
    "Affliction, destruction and demonology warlock guides for raid and mythic plus in World of Warcraft Shadowlands. Includes gear, talents, rotations and more..."
