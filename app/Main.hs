{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import Control.Monad.Trans.Either
import Data.Text

import Network.GitHub

ua :: Maybe Text
ua = Just "servant-github"

main :: IO ()
main = do
    tk <- lookupEnv "GITHUB_TOKEN"
    case tk of
        Nothing -> do
            putStrLn "Please set the GITHUB_TOKEN env variable" 
        Just token -> do
            let token' = Just (Token (pack token))
            res <- runEitherT (getOrgs ua token')
            case res of
                Left e   -> print e
                Right os -> mapM_ print os
