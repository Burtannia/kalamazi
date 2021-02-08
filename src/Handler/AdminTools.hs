{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.AdminTools where

import Import

data AdminTools = AdminTools
    { awImageManager :: Widget
    , awGuideGroups :: Widget
    , awNewGuide :: Widget
    }

mkAdminTools :: AdminTools -> Widget
mkAdminTools AdminTools {..} = do
    mcurrentRoute <- getCurrentRoute
    $(widgetFile "admin-panel")

isGuide :: Maybe (Route App) -> Bool
isGuide (Just (GuideR _)) = True
isGuide _ = False