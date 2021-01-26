module Handler.LoginRedirect where

import Import

getLoginRedirectR :: Handler ()
getLoginRedirectR = redirect $ AuthR LoginR