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
mkAdminTools AdminTools {..} = $(widgetFile "admin-panel")