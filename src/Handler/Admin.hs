{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Admin where

import Import
import Handler.Guide
import Handler.Modal
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), renderBootstrap4)