{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Admin where

import Import
import Handler.Images

getAdminR :: Handler Html
getAdminR = do
    --(_, user) <- requireAuthPair
    defaultLayout $ do
        setTitle "Admin"
        $(widgetFile "admin")

postAdminR :: Handler Html
postAdminR = do
    defaultLayout $ do
        setTitle "Admin"
        $(widgetFile "admin")