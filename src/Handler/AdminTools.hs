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
    --, awGuideGroups :: Widget
    , awNewGuide :: Widget
    , awGuideControls :: Maybe Widget -- show preview toggle if this isJust
    }

mkAdminTools :: AdminTools -> Widget
mkAdminTools AdminTools {..} = do
    $(widgetFile "admin-panel")

-- create this widget inside each handler by passing internal widgets
-- embed it in defaultLayout?