{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")        

postHomeR :: Handler Html
postHomeR = do
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")