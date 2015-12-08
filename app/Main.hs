{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Exit
import System.Environment
import Control.Monad.IO.Class
import Control.Monad
import Data.Monoid
import Data.Maybe
import Data.Text
import Data.Text.IO as T

import Network.GitHub


getAuthToken :: IO (Maybe AuthToken)
getAuthToken = do 
    mtk <- lookupEnv "GITHUB_TOKEN"
    return $ fmap (AuthToken . pack) mtk

main :: IO ()
main = do
    token <- getAuthToken
    when (not $ isJust token) $ do
        T.putStrLn "Please set the GITHUB_TOKEN env variable" 
        exitFailure

    errors <- runGitHub printOrgsAndTeams token
    case errors of
        Left e  -> liftIO $ print e
        Right _ -> exitSuccess

printOrgsAndTeams :: GitHub ()
printOrgsAndTeams = do
    os <- getOrgs 
    forM_ os $ \o -> do
        liftIO $ T.putStrLn (orgLogin o)
        teams <- orgTeams (orgLogin o)
        forM_ teams $ \t -> do
            liftIO $ T.putStrLn $ "  " <> teamName t
