{-# LANGUAGE NoImplicitPrelude #-}

module Import.Utils where

import Import.NoFoundation
import Foundation
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), renderBootstrap4)

fours :: [a] -> [[a]]
fours xs
    | length xs < 4 = [xs]
    | otherwise = take 4 xs : fours (drop 4 xs)

withIndexes :: [a] -> [(Int, a)]
withIndexes xs = zip [0..length xs - 1] xs

(-!) :: [a] -> Int -> [a]
(-!) = boundsCheck $ \n ->
    map snd . filter (not . (==) n . fst) . withIndexes

boundsCheck :: (Int -> [a] -> [a]) -> [a] -> Int -> [a]
boundsCheck f xs n
    | n < 0 = xs
    | n >= length xs = xs
    | otherwise = f n xs

boundsCheckM_ :: Monad m => [a] -> Int -> (Int -> [a] -> m ()) -> m ()
boundsCheckM_ xs n f = do
    when (n < 0) $ return ()
    when (n >= length xs) $ return ()
    f n xs

genBs4Form :: AForm Handler a -> Handler (Widget, Enctype)
genBs4Form = generateFormPost . renderBootstrap4 BootstrapBasicForm

runBs4Form :: AForm Handler a -> Handler ((FormResult a, Widget), Enctype)
runBs4Form = runFormPost . renderBootstrap4 BootstrapBasicForm

genBs4FormIdentify :: Text -> AForm Handler a -> Handler (Widget, Enctype)
genBs4FormIdentify t = generateFormPost . identifyForm t . renderBootstrap4 BootstrapBasicForm

runBs4FormIdentify :: Text -> AForm Handler a -> Handler ((FormResult a, Widget), Enctype)
runBs4FormIdentify t = runFormPost . identifyForm t . renderBootstrap4 BootstrapBasicForm