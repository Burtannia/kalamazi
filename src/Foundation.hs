{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Text.Julius          (juliusFile)
import Control.Monad.Logger (LogSource)

-- Used only when in "auth-dummy-login" setting is enabled.
import Yesod.Auth.Dummy

import Yesod.Auth.OAuth2 (oauth2Url)
import Yesod.Auth.OAuth2.Google (oauth2GoogleScoped)

import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE

import qualified Data.Text as T (append)
import qualified Network.Wai as Wai (requestHeaders)

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appImages      :: Static -- ^ Settings for user uploaded files.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , appGoogleAuthId :: Text
    , appGoogleAuthKey :: Text
    , appYouTubeKey :: Text
    }

data MenuItem = MenuItem
    { menuItemLabel :: Text
    , menuItemRoute :: Route App
    , menuItemAccessCallback :: Bool
    }

data MenuTypes
    = NavLink MenuItem
    | NavGuide MenuItem [(Text, Text)]
    | NavGroup Text Bool [MenuItem]

homeGroupName :: Text
homeGroupName = "Homepage Guides"

youtubeLink, discordLink, twitterLink, instagramLink, patreonLink, twitchLink, wagoLink, emailLink :: Text
youtubeLink = "https://www.youtube.com/kalamazigames"
discordLink = "https://discord.gg/cyn6Zsv"
twitterLink = "https://twitter.com/Kalamazii"
instagramLink = "https://instagram.com/Kalamazii"
patreonLink = "https://www.patreon.com/Kalamazi"
twitchLink = "https://www.twitch.tv/kalamazi"
wagoLink = "https://wago.io/p/kalamazi"
emailLink = "mailto:kalamazing98@gmail.com"

keywords :: Text
keywords = intercalate ","
    [ "Kalamazi"
    , "warlock"
    , "warlock guide"
    , "affliction"
    , "destruction"
    , "demonology"
    , "affliction warlock"
    , "destruction warlock"
    , "demonology warlock"
    , "guide"
    , "world of warcraft"
    , "WoW"
    , "The War Within"
    , "War Within"
    , "Nerub-ar Palace"
    , "Nerubar Palace"
    , "11.0"
    , "raid"
    , "mythic+"
    , "mythic plus"
    , "dungeons"
    , "build"
    , "talents"
    , "gear"
    , "best in slot"
    , "BiS"
    , "rotation"
    , "abilities"
    , "stat weights"
    , "weakauras"
    , "addons"
    , "pve"
    ]

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerFor App
-- type Widget = WidgetFor App ()
mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: * -> *).
    (MonadUnliftIO m) => ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    errorHandler :: ErrorResponse -> Handler TypedContent
    errorHandler errorResponse = do

        $(logWarn) (T.append "Error Response: "
                            $ pack (show errorResponse))
        req <- waiRequest
        let reqwith = lookup "X-Requested-With" $ Wai.requestHeaders req
            errorText NotFound = (404, "Not Found", "Sorry, not found")
            errorText (InternalError msg) = (400, "Bad Request", msg)
            errorText (InvalidArgs m) = (400, "Bad Request", unwords m)
            errorText (PermissionDenied msg) = (403, "Forbidden", msg)
            errorText (BadMethod _) = (405, "Method Not Allowed",
                                            "Method not supported")
            errorText NotAuthenticated = (401, "Unauthorized", "Unauthorized")

        when (maybe False (== "XMLHttpRequest") reqwith) $ do
            let (code, brief, full) = errorText errorResponse
            sendResponseStatus
                (mkStatus code brief)
                $ RepPlain $ toContent $ T.append "Error: " full
                
        case errorResponse of
            NotFound -> fmap toTypedContent $ defaultLayout $ do
                setTitle "Error 404 | Page Not Found"
                $(widgetFile "not-found")
            (PermissionDenied msg) -> fmap toTypedContent $ defaultLayout $ do
                setTitle "Error 403 | Permission Denied"
                $(widgetFile "permission-denied")
            _ -> defaultErrorHandler errorResponse

        -- -- REMOVE
        -- muser <- maybeAuthPair
        -- let isAdmin = maybe False (userIsAdmin . snd) muser

        -- if isAdmin then
        --     case errorResponse of
        --         NotFound -> fmap toTypedContent $ defaultLayout $ do
        --             setTitle "Error 404 | Page Not Found"
        --             $(widgetFile "not-found")
        --         (PermissionDenied msg) -> fmap toTypedContent $ defaultLayout $ do
        --             setTitle "Error 403 | Permission Denied"
        --             $(widgetFile "permission-denied")
        --         _ -> defaultErrorHandler errorResponse
        -- else
        --     selectRep $ provideRep $ return ("Not Found" :: Html)
        
    maximumContentLength :: App -> Maybe (Route App) -> Maybe Word64
    maximumContentLength _ _ = Just $ 5 * 1024 * 1024 -- 5 megabytes

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = sslOnlySessions $
        Just <$> defaultClientSessionBackend
        720    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware . (sslOnlyMiddleware 720)

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        muser <- maybeAuthPair

        mcurrentRoute <- getCurrentRoute

        let isAdmin = maybe False (userIsAdmin . snd) muser
        
        guideGroups <- fmap (map entityVal) $
            liftHandler $ runDB $ selectList [] [Asc GuideGroupPosition]

        let mkGuideLink (guideId, guide, sectionUrls) =
                let mi = MenuItem
                        (fromMaybe (guideTitle guide) $ guideShortTitle guide)
                        (GuideR guideId) (isAdmin || guideIsPublished guide)
                 in (mi, sectionUrls)
                
            mkGroupNav gg = liftHandler $ do
                let getSections (guideId, g) = do
                        sections <- mapM (runDB . getJust) $ guideSections g
                        let f (x,y) = if y == "Banner" then ("", "Full Guide") else (x,y)
                            urls = map (f . (sectionUrl &&& sectionTitle)) sections
                        return (guideId, g, urls)

                guides' <- mapM (sequence . (id &&& runDB . getJust)) $ guideGroupGuides gg
                guides <- mapM getSections guides'

                let guideLinks = filter (menuItemAccessCallback . fst) $ map mkGuideLink guides 
                    shouldShow = length guideLinks > 0
                return $
                    case guideLinks of
                        [(mi, [])] -> NavLink (mi { menuItemLabel = guideGroupName gg })
                        [(mi, [_])] -> NavLink (mi { menuItemLabel = guideGroupName gg })
                        [(mi, secUrls)] -> NavGuide (mi { menuItemLabel = guideGroupName gg }) secUrls
                        _  -> NavGroup (guideGroupName gg) shouldShow $ map fst guideLinks

        ggLinks <- mapM mkGroupNav guideGroups

        -- Define the menu items of the header.
        let menuItems =
                (NavLink $ MenuItem "Home" HomeR True)
                : ggLinks ++
                -- [ NavLink $ MenuItem "Login" (AuthR LoginR) (isNothing muser)
                -- , NavLink $ MenuItem "Logout" (AuthR LogoutR) (isJust muser)
                -- ]
                [NavLink $ MenuItem "Logout" (AuthR LogoutR) (isJust muser)]

            getCallback (NavLink mi) = menuItemAccessCallback mi
            getCallback (NavGuide mi _) = menuItemAccessCallback mi
            getCallback (NavGroup _ cb _) = cb

            getLabel (NavLink (MenuItem l _ _)) = l
            getLabel (NavGuide (MenuItem l _ _) _) = l
            getLabel (NavGroup l _ _) = l

            filteredMenuItems =
                let xs = [x | x <- menuItems, getCallback x, getLabel x /= homeGroupName]
                in zip [0..length xs - 1] xs

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            for_ (appAnalytics $ appSettings master) $ \gaCode ->
                toWidget $(juliusFile "templates/analytics.julius")
            for_ (appAdsense $ appSettings master) $ \adsCode ->
                toWidget $(juliusFile "templates/adsense.julius")
            $(widgetFile "default-layout")

        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute
        :: App
        -> Maybe (Route App)
    authRoute _ = Just $ AuthR $ oauth2Url "google"

    isAuthorized
        :: Route App  -- ^ The route the user is visiting.
        -> Bool       -- ^ Whether or not this is a "write" request.
        -> Handler AuthResult
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized LoginRedirectR _ = return Authorized

    isAuthorized HomeR False = return Authorized
    isAuthorized HomeR _ = isAuthenticated

    isAuthorized (ImagesR _) _ = return Authorized
    isAuthorized (ImageR _) _ = isAuthenticated

    isAuthorized (GuideR _) False = return Authorized
    isAuthorized (GuideR _) True = isAuthenticated
    
    isAuthorized (GuideGroupR _) _ = isAuthenticated

    isAuthorized (SectionR _) _ = isAuthenticated

    isAuthorized PrivacyR _ = return Authorized

    isAuthorized OnlyFansR _ = return Authorized

    isAuthorized PokeR _ = isAuthenticated

    isAuthorized DiscordR _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger

-- Define breadcrumbs.
instance YesodBreadcrumbs App where
    -- Takes the route that the user is currently on, and returns a tuple
    -- of the 'Text' that you want the label to display, and a previous
    -- breadcrumb route.
    breadcrumb
        :: Route App  -- ^ The route the user is visiting currently.
        -> Handler (Text, Maybe (Route App))
    breadcrumb HomeR = return ("Home", Nothing)
    breadcrumb (AuthR _) = return ("Login", Just HomeR)
    breadcrumb  _ = return ("home", Nothing)

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest :: App -> Route App
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest :: App -> Route App
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer :: App -> Bool
    redirectToReferer _ = True

    loginHandler = do
        ma <- maybeAuthId
        when (isJust ma) $ do
            setMessage "You are already logged in"
            redirect HomeR
        redirect $ AuthR $ oauth2Url "google"

    authenticate :: (MonadHandler m, HandlerSite m ~ App)
                 => Creds App -> m (AuthenticationResult App)
    authenticate creds = liftHandler $ runDB $ do
        x <- getBy $ UniqueIdent $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Authenticated uid
            Nothing -> Authenticated <$> insert User
                { userIdent = credsIdent creds
                , userIsAdmin = False
                }

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins :: App -> [AuthPlugin App]
    authPlugins app = [ oauth2GoogleScoped ["email", "profile"]
        (appGoogleAuthId app) (appGoogleAuthKey app) ]
        ++ extraAuthPlugins
        -- Enable authDummy login if enabled.
        where extraAuthPlugins = [authDummy | appAuthDummyLogin $ appSettings app]

-- | Access function to determine if a user is logged in and is an admin.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    -- user <- requireAuth
    -- if userIsAdmin $ entityVal user
    --     then return Authorized
    --     else notFound
    muser <- maybeAuth
    return $ case muser of
        Nothing -> Unauthorized "You must be logged in to access this page"
        Just (Entity _ user)
            | userIsAdmin user -> Authorized
            | otherwise -> Unauthorized "You are not authorized to access this page"

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

msgRedirect :: Html -> Handler ()
msgRedirect msg = do
    mcurrentRoute <- getCurrentRoute
    setMessage msg
    case mcurrentRoute of
        Nothing -> redirect HomeR
        Just r -> redirect r

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
