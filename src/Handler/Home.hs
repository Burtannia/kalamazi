{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Handler.Images
import Handler.Guide
import Handler.AdminTools

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        let madminTools = Just $ mkAdminTools $ AdminTools
                                                getImageManager
                                                ggManager
                                                genNewGuide
                                                Nothing
        $(widgetFile "homepage")        

postHomeR :: Handler Html
postHomeR = do
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        let madminTools = Just $ mkAdminTools $ AdminTools
                                                postImageManager
                                                ggManager
                                                runNewGuide
                                                Nothing
        $(widgetFile "homepage")