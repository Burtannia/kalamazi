{-# LANGUAGE NoImplicitPrelude #-}

module Import.Utils where

import Import.NoFoundation
import Foundation
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), renderBootstrap4)

fours :: [a] -> [[a]]
fours xs
    | length xs < 4 = [xs]
    | otherwise = take 4 xs : fours (drop 4 xs)

genBs4Form :: AForm Handler a -> Handler (Widget, Enctype)
genBs4Form = generateFormPost . renderBootstrap4 BootstrapBasicForm

runBs4Form :: AForm Handler a -> Handler ((FormResult a, Widget), Enctype)
runBs4Form = runFormPost . renderBootstrap4 BootstrapBasicForm

genBs4FormIdentify :: Text -> AForm Handler a -> Handler (Widget, Enctype)
genBs4FormIdentify t = generateFormPost . identifyForm t . renderBootstrap4 BootstrapBasicForm

runBs4FormIdentify :: Text -> AForm Handler a -> Handler ((FormResult a, Widget), Enctype)
runBs4FormIdentify t = runFormPost . identifyForm t . renderBootstrap4 BootstrapBasicForm