{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Import.Utils where

import Import.NoFoundation
import Foundation
import qualified Data.List as L (tail)
import Data.Time.Clock (NominalDiffTime)
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), renderBootstrap4)

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
mkFormId ts = foldr (<>) "" $ intersperse "" ts

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
genBs4Form = generateFormPost . renderBootstrap4 BootstrapBasicForm

runBs4Form :: AForm Handler a -> Handler ((FormResult a, Widget), Enctype)
runBs4Form = runFormPost . renderBootstrap4 BootstrapBasicForm

genBs4FormIdentify :: Text -> AForm Handler a -> Handler (Widget, Enctype)
genBs4FormIdentify t = generateFormPost . identifyForm t . renderBootstrap4 BootstrapBasicForm

runBs4FormIdentify :: Text -> AForm Handler a -> Handler ((FormResult a, Widget), Enctype)
runBs4FormIdentify t = runFormPost . identifyForm t . renderBootstrap4 BootstrapBasicForm

convertFieldPair :: (c -> a)
    -> (c -> b)
    -> (a -> b -> c)
    -> Field Handler a
    -> Field Handler b
    -> Field Handler c
convertFieldPair toA toB toC fa fb = Field
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
            <div ##{ti}>
                ^{viewA (ti <> "-A") tn as (fmap toA eRes) req}
                ^{viewB (ti <> "-B") tn as (fmap toB eRes) req}
        |]
    , fieldEnctype = fieldEnctype fa
    }