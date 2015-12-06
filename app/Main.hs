{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import Control.Monad.Trans.Either
import Control.Monad
import Data.Monoid
import Data.Maybe
import Data.Text
import Data.Text.IO as T

import Network.GitHub

ua :: Maybe Text
ua = Just "servant-github"

getToken :: IO (Maybe Token)
getToken = do 
    mtk <- lookupEnv "GITHUB_TOKEN"
    return $ fmap (Token . pack) mtk

main :: IO ()
main = do
    token <- getToken
    when (not $ isJust token) $ do
        T.putStrLn "Please set the GITHUB_TOKEN env variable" 
        return ()
    res <- runEitherT (getOrgs ua token)
    case res of
        Left e   -> print e
        Right os -> mapM_ process os

process :: Organisation -> IO ()
process o = do
    token <- getToken
    T.putStrLn (orgLogin o)
    res <- runEitherT (orgTeams ua token (orgLogin o))
    case res of
        Left e   -> print e
        Right ts -> mapM_ printTeam ts

printTeam :: Team -> IO()
printTeam = T.putStrLn . ("  " <>) . teamName
    
