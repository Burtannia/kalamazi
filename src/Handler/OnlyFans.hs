{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.OnlyFans where

import Import

getOnlyFansR :: Handler Html
getOnlyFansR = do
    defaultLayout $ do
        setTitle "OnlyFans"
        setLogoMetaImage
        $(widgetFile "onlyfans")