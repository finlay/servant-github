{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Exit
import System.Environment
import Control.Monad.IO.Class
import Control.Monad
import Data.Monoid
import Data.Maybe
import Data.Text as T
import Data.Text.IO as T

import Network.GitHub

main :: IO ()
main = do

    -- Get AuthToken from environment variable
    token <- fmap (AuthToken . pack) <$> lookupEnv "GITHUB_TOKEN"

    -- Require the token for this to work
    when (not $ isJust token) $ do
        T.putStrLn "Please set the GITHUB_TOKEN env variable" 
        exitFailure

    -- Run GitHub computation, print errors if there are any
    errors <- runGitHub (printTeams "dragonfly-science") token
    case errors of
        Left e  -> liftIO $ print e
        Right _ -> exitSuccess

printOrgsAndTeams :: GitHub ()
printOrgsAndTeams = do
    os <- userOrganisations
    forM_ os $ \o -> do
        liftIO $ T.putStrLn (orgLogin o)
        teams <- organisationTeams (orgLogin o)
        forM_ teams $ \t -> do
            liftIO $ T.putStrLn $ "  " <> teamName t
            members <- teamMembers (teamId t)
            liftIO $ T.putStrLn $ "    " <> T.intercalate ", " [ memberLogin m | m <- members]
            repos <- teamRepositories (teamId t)
            liftIO $ T.putStrLn $ "    " <> T.intercalate ", " [ repositoryName r | r <- repos]

printTeams :: OrgLogin -> GitHub ()
printTeams ol = do
    teams <- organisationTeams ol
    forM_ teams $ \t -> do
        liftIO $ T.putStrLn $ "  " <> teamName t
        members <- teamMembers (teamId t)
        liftIO $ T.putStrLn $ "    " <> T.intercalate ", " [ memberLogin m | m <- members]
        repos <- teamRepositories (teamId t)
        liftIO $ T.putStrLn $ "    " <> T.intercalate ", " [ repositoryName r | r <- repos]
