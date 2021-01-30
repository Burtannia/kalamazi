{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Import.Utils where

import Import.NoFoundation
import Foundation
import qualified Data.List as L (tail)
import Data.Time.Clock (NominalDiffTime)
import Yesod.Form.MultiInput (MultiSettings (..))
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), renderBootstrap4)
import qualified Data.List as L (init)

dropExt :: Text -> Text
dropExt = pack . go . splitDots . unpack
    where
        go xs
            | length xs < 3 = concat xs
            | otherwise = concat $ L.init $ L.init xs

splitDots :: String -> [String]
splitDots = go ""
    where
        go _ "" = [""]
        go buffer ('.' : cs) = (reverse buffer) : "." : go "" cs
        go buffer (c:cs) = go (c : buffer) cs

bs4LISettings :: MultiSettings App
bs4LISettings = MultiSettings
    "btn btn-secondary"
    "btn btn-danger"
    "form-text text-muted"
    "has-error"
    addIcon delIcon (Just errW)
    where
        addIcon = Just [shamlet|<i .lnir .lnir-plus>|]
        delIcon = Just [shamlet|<i .lnir .lnir-trash-can>|]
        errW err =
            [whamlet|
                <div .invalid-feedback>#{err}
            |]

withClass :: Text -> FieldSettings App -> FieldSettings App
withClass t fs = fs {fsAttrs = addClass t $ fsAttrs fs}

withPlaceholder :: Text -> FieldSettings App -> FieldSettings App
withPlaceholder t fs = fs {fsAttrs = ("placeholder", t) : fsAttrs fs}

withTooltip :: SomeMessage App -> FieldSettings App -> FieldSettings App
withTooltip tt fs = fs {fsTooltip = Just tt}

formatDiffTime :: NominalDiffTime -> String
formatDiffTime dt = go (floor $ toRational dt) incs
    where
        go :: Int -> [(Int, String, String)] -> String
        go _ [] = "formatDiffTime: this should never happen"
        go t ((x, sing, plur) : xs) =
            case t `divMod` x of
                (0, 1) -> "1" ++ sing
                (0, n) -> show n ++ plur
                (m, _) -> go m xs
        incs :: [(Int, String, String)]
        incs =
            [ (60, " second", " seconds")
            , (60, " minute", " minutes")
            , (24, " hour", " hours")
            , (30, " day", " days")
            , (12, " month", " months")
            , (1, " year", " years")
            ]

moveForward :: Eq a => a -> [a] -> [a]
moveForward _ [] = []
moveForward _ [x] = [x]
moveForward x (y:z:ys)
    | x == y    = z : y : ys
    | otherwise = y : moveForward x (z:ys)

moveBackward :: Eq a => a -> [a] -> [a]
moveBackward _ [] = []
moveBackward _ [x] = [x]
moveBackward x (y:z:ys)
    | x == z    = z : y : ys
    | otherwise = y : moveBackward x (z:ys)

mkFormId :: [Text] -> Text
mkFormId ts = foldr (<>) "" $ intersperse "-" ts

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

fours :: [a] -> [[a]]
fours xs
    | length xs < 4 = [xs]
    | otherwise = take 4 xs : fours (drop 4 xs)

withIndexes :: [a] -> [(Int, a)]
withIndexes xs = zip [0..] xs

(-!) :: [a] -> Int -> [a]
(-!) xs n = [ x | (i,x) <- withIndexes xs, not $ i == n ]

(-=!) :: Eq a => [a] -> a -> [a]
(-=!) xs y = [x | x <- xs, not $ x == y]

(/!) :: [a] -> (a, Int) -> [a]
(/!) xs (y, n) = [ if i == n then y else x | (i,x) <- withIndexes xs ]

mkOptions :: Text -> [(Text, a)] -> OptionList a
mkOptions prefix xs = mkOptionList opts
    where
        mkOption (ix, (disp, val)) = Option disp val $ prefix <> tshow ix
        opts = map mkOption $ withIndexes xs

isSuccess :: FormResult a -> Bool
isSuccess (FormSuccess _) = True
isSuccess _ = False

boundsCheck :: (Int -> [a] -> [a]) -> [a] -> Int -> [a]
boundsCheck f xs n
    | n < 0 = xs
    | n >= length xs = xs
    | otherwise = f n xs

boundsCheckM :: Monad m => [a] -> Int -> m b -> m (Either Text b)
boundsCheckM xs n mb
    | n < 0 = return $ Left "Index out of bounds (negative)"
    | n >= length xs = return $ Left "Index out of bounds"
    | otherwise = liftM Right mb
    
genBs4Form :: AForm Handler a -> Handler (Widget, Enctype)
genBs4Form = genBs4Form' BootstrapBasicForm

runBs4Form :: AForm Handler a -> Handler ((FormResult a, Widget), Enctype)
runBs4Form = runBs4Form' BootstrapBasicForm

genBs4Form' :: BootstrapFormLayout -> AForm Handler a -> Handler (Widget, Enctype)
genBs4Form' formType = generateFormPost . renderBootstrap4 formType

runBs4Form' :: BootstrapFormLayout -> AForm Handler a -> Handler ((FormResult a, Widget), Enctype)
runBs4Form' formType = runFormPost . renderBootstrap4 formType

genBs4FormIdentify :: Text -> AForm Handler a -> Handler (Widget, Enctype)
genBs4FormIdentify = genBs4FormIdentify' BootstrapBasicForm

runBs4FormIdentify :: Text -> AForm Handler a -> Handler ((FormResult a, Widget), Enctype)
runBs4FormIdentify = runBs4FormIdentify' BootstrapBasicForm

genBs4FormIdentify' :: BootstrapFormLayout
    -> Text
    -> AForm Handler a
    -> Handler (Widget, Enctype)
genBs4FormIdentify' formType t = generateFormPost . identifyForm t . renderBootstrap4 formType

runBs4FormIdentify' :: BootstrapFormLayout
    -> Text
    -> AForm Handler a
    -> Handler ((FormResult a, Widget), Enctype)
runBs4FormIdentify' formType t = runFormPost . identifyForm t . renderBootstrap4 formType

convertFieldPair :: (c -> a)
    -> (c -> b)
    -> (a -> b -> c)
    -> Field Handler a
    -> Field Handler b
    -> Text
    -> Field Handler c
convertFieldPair toA toB toC fa fb wrapClass = Field
    { fieldParse = \rawVals fileVals -> do
        let parseA = fieldParse fa
            parseB = fieldParse fb

        eResA <- parseA rawVals fileVals
        eResB <- parseB (safeTail rawVals) (safeTail fileVals)

        return $ liftA2 (liftA2 toC) eResA eResB

    , fieldView = \ti tn as eRes req -> do
        let viewA = fieldView fa
            viewB = fieldView fb
        [whamlet|
            <div ##{ti} class=#{wrapClass}>
                ^{viewA (ti <> "-A") tn as (fmap toA eRes) req}
                ^{viewB (ti <> "-B") tn as (fmap toB eRes) req}
        |]
    , fieldEnctype = fieldEnctype fa
    }