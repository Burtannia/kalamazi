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

    ytVideo <- getLatestVideo

    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        let madminTools =
                if isAdmin then
                    Just $ mkAdminTools $ 
                        AdminTools
                        getImageManager
                        ggManager
                        genNewGuide
                        Nothing
                else
                    Nothing
        $(widgetFile "homepage")        

postHomeR :: Handler Html
postHomeR = do
    muser <- maybeAuth
    let isAdmin = maybe False (userIsAdmin . entityVal) muser

    ytVideo <- getLatestVideo

    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        let madminTools =
                if isAdmin then
                    Just $ mkAdminTools $ 
                        AdminTools
                        postImageManager
                        ggManager
                        runNewGuide
                        Nothing
                else
                    Nothing
        $(widgetFile "homepage")

