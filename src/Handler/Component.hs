{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Component where

import Import
import Handler.Images
import Handler.Modal
import Summernote
import Data.Bitraversable (bisequence)
import Text.Julius (rawJS)
import Data.Aeson.Types ()
import Data.Time.Format.ISO8601
import qualified Data.Text.IO as TIO (readFile, writeFile)
import System.Directory (removeFile)
import Yesod.Form.Bootstrap4 (bfs)

genNewComponent :: [Entity Image] -> SectionId -> Widget
genNewComponent imgs sectionId = do
    modalId <- newIdent
    let formWidgets = map (uncurry genForm) $ withIndexes comps
    $(widgetFile "components/new-component")
    where
        genForm ix (_, compId, cc) = do
            let isFirst = ix == 0
            formId <- newIdent
            (formWidget, enctype) <- liftHandler
                $ genBs4FormIdentify (mkCreateCompId sectionId compId)
                $ createCompForm imgs cc
            $(widgetFile "components/new-component-form")

runNewComponent :: [Entity Image] -> SectionId -> Handler (Widget, Maybe Component)
runNewComponent imgs sectionId = do
    modalId <- newIdent
    (formWidgets, mcomps) <- fmap unzip $
            mapM (uncurry runForm) $ withIndexes comps
    let widget = $(widgetFile "components/new-component")
    return (widget, listToMaybe $ catMaybes mcomps)
    where
        runForm ix (_, compId, cc) = do
            let isFirst = ix == 0
            formId <- newIdent
            ((formRes, formWidget), enctype) <- liftHandler
                $ runBs4FormIdentify (mkCreateCompId sectionId compId)
                $ createCompForm imgs cc

            mr <- case formRes of
                FormSuccess compData ->
                    fmap Just $ mkComponent compData

                FormMissing -> return Nothing

                FormFailure errs -> do
                    liftIO $ putStrLn "runNewComponent"
                    print errs
                    return Nothing

            let widget = $(widgetFile "components/new-component-form")
            return (widget, mr)

mkCreateCompId :: SectionId -> Text -> Text
mkCreateCompId sectionId t = mkFormId ["create", t, toPathPiece sectionId]

mkEditCompId :: SectionId -> Text -> Text
mkEditCompId sectionId t = mkFormId ["edit", t, toPathPiece sectionId]

mkCompTabId :: SectionId -> Text -> Text
mkCompTabId sectionId compId = "sec-" <> toPathPiece sectionId <> "-" <> compId <> "-create"

-- A form friendly representation of a component to be constructed.
data CreateComponent
    = CreateMarkup (Maybe Html)
    | CreateToggleText (Maybe SpaceChar) [(Text, Html)]
    | CreateToggleImage [(ImageId, Html)]
    | CreateImage (Maybe ImageId)
    | CreateVideo (Maybe Text)
    | CreateWeakAura (Maybe Text) (Maybe Textarea)
    | CreateDivider (Maybe Axis) (Maybe Bool)

comps :: [(Text, Text, CreateComponent)]
comps =
    [ ("Markup", "markup", CreateMarkup Nothing)
    , ("Toggle Texts", "toggletext", CreateToggleText Nothing [])
    , ("Toggle Images", "toggleimage", CreateToggleImage [])
    , ("Image", "image", CreateImage Nothing)
    , ("Video", "video", CreateVideo Nothing)
    , ("WeakAura", "weakaura", CreateWeakAura Nothing Nothing)
    , ("Divider", "divider", CreateDivider Nothing Nothing)
    ]

toCreateComp :: Component -> Handler CreateComponent
toCreateComp (CMarkup mId) = CreateMarkup
    <$> fmap (Just . markupBlockContent) (runDB $ getJust mId)
toCreateComp (CToggle (ToggleTexts sc ts)) = CreateToggleText
    <$> pure (Just sc)
    <*> mapM getMarkup ts
toCreateComp (CToggle (ToggleImages ts)) = CreateToggleImage
    <$> mapM getMarkup ts
toCreateComp (CImage imgId) = return $ CreateImage $ Just imgId
toCreateComp (CVideo url) = return $ CreateVideo $ Just url
toCreateComp (CWeakAura wId) = do
    wa <- runDB $ getJust wId
    let waTitle = weakAuraTitle wa
    fPath <- mkWeakauraPath $ iso8601Show $ weakAuraCreated wa
    waContent <- liftIO $ TIO.readFile fPath
    return $ CreateWeakAura (Just waTitle) (Just $ Textarea waContent)
toCreateComp (CDivider axis visible) = return $ CreateDivider (Just axis) (Just visible)

getMarkup :: (a, MarkupBlockId) -> Handler (a, Html)
getMarkup = sequence . fmap (fmap markupBlockContent . runDB . getJust)

data ComponentData
    = CD_Markup Html
    | CD_ToggleText SpaceChar [(Text, Html)]
    | CD_ToggleImage [(ImageId, Html)]
    | CD_Image ImageId
    | CD_Video Text
    | CD_WeakAura Text Textarea
    | CD_Divider Axis Bool

mkComponent :: ComponentData -> Handler Component
mkComponent (CD_Markup m) = do
    mId <- runDB $ insert $ MarkupBlock m
    return $ CMarkup mId
mkComponent (CD_ToggleText sc ts') = do
    ts <- mapM (traverse (runDB . insert . MarkupBlock)) ts'
    return $ CToggle $ ToggleTexts sc ts
mkComponent (CD_ToggleImage ts') = do
    ts <- mapM (traverse (runDB . insert . MarkupBlock)) ts'
    return $ CToggle $ ToggleImages ts
mkComponent (CD_Image imgId) = return $ CImage imgId
mkComponent (CD_Video url) = return $ CVideo url
mkComponent (CD_WeakAura title content) = do
    now <- liftIO getCurrentTime
    
    fPath <- mkWeakauraPath $ iso8601Show now
    liftIO $ TIO.writeFile fPath (unTextarea content)

    wId <- runDB $ insert $ WeakAura title now

    return $ CWeakAura wId
mkComponent (CD_Divider axis visible) = return $ CDivider axis visible

createCompForm :: [Entity Image] -> CreateComponent -> AForm Handler ComponentData
createCompForm _ (CreateMarkup mhtml) = CD_Markup
    <$> areq snField (bfs ("Content" :: Text)) mhtml

createCompForm _ (CreateToggleText msc ts) = CD_ToggleText
    <$> areq (radioFieldList spaceChars) (withClass "mr-2 lg-radio" $ "Space Character") msc
    <*> amulti toggleField groupSettings ts 0 bs4LISettings
    where
        spaceChars = [ ("| - Vertical Bar" :: Text, SpaceLine)
                     , ("> - Chevron", SpaceChev)
                     ]
        toggleField = convertFieldPair
            fst snd (,) textField snField "w-100"
        groupSettings = FieldSettings
            { fsLabel = "Groups"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control mb-2")
                , ("placeholder", "Group Label") ]
            }

createCompForm imgs (CreateToggleImage ts) = CD_ToggleImage
    <$> amulti toggleField (bfs ("Groups" :: Text)) ts 0 bs4LISettings
    where
        toggleField = convertFieldPair
            fst snd (,) (imageSelectFieldToggle imgs) snField "image-group"

createCompForm imgs (CreateImage mimg) = CD_Image
    <$> areq (imageSelectField imgs) (bfs ("Image" :: Text)) mimg

createCompForm _ (CreateVideo murl) = CD_Video
    <$> areq ytUrlField (withPlaceholder ph $ withTooltip vidTip $ bfs ("Url" :: Text)) murl
    where
        ytUrlField = check checkNoCookie urlField
        checkNoCookie :: Text -> Either Text Text
        checkNoCookie x
            | "youtube-nocookie" `isInfixOf` x = Right x
            | "youtube" `isInfixOf` x = Left "Please use \"youtube-nocookie.com\" instead of \"youtube.com\""
            | otherwise = Right x
        ph = "https://youtube-nocookie.com/embed/<video-id>"
        vidTip = fromString $
            "In order to embed videos from YouTube, the URL must have the following format:"
            <> " https://youtube-nocookie.com/embed/<video-id>"

createCompForm _ (CreateWeakAura mtitle mcontent) = CD_WeakAura
    <$> areq textField (withPlaceholder "My WeakAura" $ withClass "mb-1" $ bfs ("Title" :: Text)) mtitle
    <*> areq textareaField (withPlaceholder ph $ withClass "minh-12rem" $ bfs ("Content" :: Text)) mcontent
    where
        ph = "Paste WeakAura, addon profile or macro content..."

createCompForm _ (CreateDivider maxis mvisible) = CD_Divider
    <$> areq (radioFieldList axisOpts) (withClass "mr-2 lg-radio" $ "Axis") maxis
    <*> areq checkBoxField (withTooltip visTip $ withClass "lg-checkbox" "Visible") mvisible
    where
        axisOpts = [ ("Column" :: Text, Column)
                   , ("Row", Row)
                   ]
        visTip = "If visible then the divider will show as a line rather than just affecting the layout."

mkWeakauraPath :: String -> Handler FilePath
mkWeakauraPath name = do
    app <- getYesod
    let weakauraDir = appWeakauraDir $ appSettings app
    return $ weakauraDir <> "/" <> name <> ".txt"

deleteComponent :: Component -> Handler ()
deleteComponent (CMarkup markupId) = runDB $ delete markupId
deleteComponent (CToggle t) = case t of
    ToggleTexts _ toggles -> deleteToggles toggles
    ToggleImages toggles -> deleteToggles toggles
    where
        deleteToggles :: [ToggleOption a] -> Handler ()
        deleteToggles = mapM_ $ runDB . delete . snd
deleteComponent (CImage _) = return ()
deleteComponent (CVideo _) = return ()
deleteComponent (CWeakAura wId) = do
    wa <- liftHandler $ runDB $ getJust wId
    fPath <- mkWeakauraPath $ iso8601Show $ weakAuraCreated wa
    liftIO $ removeFile fPath
    runDB $ delete wId
deleteComponent (CDivider _ _) = return ()

getCompWidget :: [Entity Image] -> Bool -> SectionId -> Int -> Component -> Widget
getCompWidget imgs isAdmin sectionId ix comp = do
    compId <- newIdent
    cc <- liftHandler $ toCreateComp comp

    -- generate edit form
    form <- liftHandler
        $ genBs4FormIdentify (mkEditCompId sectionId $ tshow ix)
        $ createCompForm imgs cc

    -- generate controls and display widget
    let canMove = compCanMove comp
        controls = compControls sectionId compId canMove form
        compWidget = displayComponent comp
        isCol = isDivCol comp
        isRow = isDivRow comp

    $(widgetFile "component")

postCompWidget :: [Entity Image] -> Bool -> SectionId -> Int -> Component
    -> Handler (Widget, Maybe (Component, Int))
postCompWidget imgs isAdmin sectionId ix comp = do
    compId <- newIdent
    cc <- toCreateComp comp
    ((formRes, formWidget), enctype) <- liftHandler
        $ runBs4FormIdentify (mkEditCompId sectionId $ tshow ix)
        $ createCompForm imgs cc
    let canMove = compCanMove comp
        controls = compControls sectionId compId canMove (formWidget, enctype)
        compWidget = displayComponent comp
        isCol = isDivCol comp
        isRow = isDivRow comp

    mr <- case formRes of
        FormSuccess compData -> do
            cd <- mkComponent compData
            deleteComponent comp
            return $ Just (cd, ix)

        FormMissing -> return Nothing

        FormFailure errs -> do
            liftIO $ putStrLn "postCompWidget"
            print errs
            return Nothing

    let widget = $(widgetFile "component")
    return (widget, mr)

compCanMove :: Component -> Bool
compCanMove x = not $ isDivCol x || isDivRow x

compControls :: SectionId -> Text -> Bool -> (Widget, Enctype) -> Widget
compControls sectionId compId canMove f = do
    delId <- newIdent
    upId <- newIdent
    downId <- newIdent
    let editWidget = mkModalEdit "Edit Component" f
    $(widgetFile "components/controls")

displayComponent :: Component -> Widget
displayComponent = displayComponent'
    where
        displayComponent' (CMarkup markupId) = do
            markup <- liftHandler $ runDB $ getJust markupId
            displayMarkup markup
        
        displayComponent' (CToggle (ToggleTexts sc ts')) = do
            ts <- flip mapM ts' $ liftHandler
                                . sequence
                                . fmap (runDB . getJust)
            let headers = map (mkTextSnippet . preEscapedToMarkup . fst) ts
                headerClass = "toggle-text" :: Text
                mSpaceChar = Just $ case sc of
                    SpaceLine -> ("|" :: Text)
                    SpaceChev -> ">"
            toggleId <- newIdent
            $(widgetFile "components/toggle")

        displayComponent' (CToggle (ToggleImages ts')) = do
            ts <- flip mapM ts' $ liftHandler
                                . bisequence
                                . bimap mkImageSnippet (runDB . getJust)
            let headers = map fst ts
                headerClass = "toggle-image" :: Text
                mSpaceChar = Nothing :: Maybe Text
            toggleId <- newIdent
            $(widgetFile "components/toggle")
        
        displayComponent' (CImage imgId) =
            [whamlet|<img .img-fluid src=@{ImagesR $ mkImageUrl imgId}>|]

        displayComponent' (CVideo url) =
            [whamlet|
                <div .video-comp>
                    <div .iframe-wrapper>
                        <iframe src=#{url} frameborder="0" allowfullscreen="true" scrolling="no">
            |]

        displayComponent' (CWeakAura wId) = do
            wa <- liftHandler $ runDB $ getJust wId
            fPath <- liftHandler $ mkWeakauraPath $ iso8601Show $ weakAuraCreated wa
            waContent <- liftHandler $ liftIO $ TIO.readFile fPath
            waId <- newIdent
            $(widgetFile "components/weakaura")

        displayComponent' (CDivider axis visible) = $(widgetFile "components/divider")

        mkTextSnippet mu = [shamlet| <h6>#{mu} |]

        mkImageSnippet imgId = withUrlRenderer
            [hamlet|<img src=@{ImagesR $ mkImageUrl imgId}>|]

        displayMarkup markup = $(widgetFile "components/markup")

type CompCol = [Component]
type CompRow = [CompCol]

isDivType :: Axis -> Component -> Bool
isDivType ax (CDivider ax' _) = ax == ax'
isDivType _ _ = False

isDivCol :: Component -> Bool
isDivCol = isDivType Column

isDivRow :: Component -> Bool
isDivRow = isDivType Row

layoutComps :: [Component] -> [CompRow]
layoutComps xs = map (splitWhenKeep isDivCol) rows
    where
        rows = splitWhenKeep isDivRow xs

markDivs :: [[[Widget]]] -> [( [( [Widget] , Bool )] , Bool )]
markDivs xs = zip ys alts
    where
        alts = intersperse True $ repeat False
        ys = map (flip zip alts) xs