{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Network.GitHub.Client
where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Either
import Data.Proxy
import GHC.TypeLits
import Data.Text

import Servant.API
import Servant.Client

import Network.HTTP.Link.Types
import Network.HTTP.Link.Parser (parseLinkHeaderBS)

import Network.GitHub.Authentication

host :: BaseUrl
host = BaseUrl Https "api.github.com" 443

type UserAgent = Text
type GitHub = ReaderT (Maybe AuthToken) (StateT Pagination (EitherT ServantError IO))

runGitHub :: GitHub a -> Maybe AuthToken -> IO (Either ServantError a)
runGitHub comp token = runEitherT $ evalStateT (runReaderT comp token) defPagination

-- | Issues supported in monad automatically
-- User-Agent, Auth Token, Pagination Headers

type family AddHeaders a :: * where
    AddHeaders ((sym :: Symbol) :> last) 
        = (sym :: Symbol) :> AddHeaders last
    AddHeaders (first :> last)
        = first :> AddHeaders last
    AddHeaders last 
        = Header "User-Agent" Text :> OAuth2Token :> ReadHeaders last
type family ReadHeaders a :: * where
    ReadHeaders (Get cts [res])    
        = QueryParam "page" Int :> QueryParam "per_page" Int 
          :> Get cts (Headers '[Header "Link" Text] [res])
    ReadHeaders (Post cts [res])   
        = QueryParam "page" Int :> QueryParam "per_page" Int 
          :> Post cts (Headers '[Header "Link" Text] [res])
    ReadHeaders (Delete cts [res]) 
        = QueryParam "page" Int :> QueryParam "per_page" Int 
          :> Delete cts (Headers '[Header "Link" Text] [res])
    ReadHeaders (Put cts [res])    
        = QueryParam "page" Int :> QueryParam "per_page" Int 
          :> Put cts (Headers '[Header "Link" Text] [res])
    ReadHeaders (Patch cts [res])  
        = QueryParam "page" Int :> QueryParam "per_page" Int 
          :> Patch cts (Headers '[Header "Link" Text] [res])
    ReadHeaders otherwise = otherwise

type Pre a = Maybe UserAgent -> Maybe AuthToken 
            -> EitherT ServantError IO a

type Pres a = Maybe UserAgent -> Maybe AuthToken 
            -> Maybe Int -> Maybe Int
            -> EitherT ServantError IO (Headers '[Header "Link" Text] [a])

type family GHF a :: * where
    GHF (Pre a)  = GitHub a
    GHF (Pres a) = GitHub [a]
    GHF (a -> b) = a -> GHF b

class HasGitHub a where
    embedGitHub :: UserAgent -> a -> GHF a
instance HasGitHub (Pres a) where
    embedGitHub ua comp = do
        token <- ask
        lift $ do
            rec <- gets recurse
            if rec 
                then modify $ \pg -> pg { page = 1, links = Nothing }
                else return ()
                        
        let accumPages acc = do
             p  <- gets page
             pp <- gets perPage
             hres <- lift $ comp (Just ua) token (Just p) (Just pp)
             case getHeaders hres of
                 [("Link", lks)] -> modify $ \pg -> pg {links = (parseLinkHeaderBS lks)}
                 _ -> return ()
             let acc' = acc ++ getResponse hres
             rec <- gets recurse
             next <- gets hasNextLink
             if rec && next 
                 then do
                     modify $ \pg -> pg {page = p + 1}
                     accumPages acc'
                 else return acc' 
        lift $ accumPages []

instance HasGitHub (Pre a) where
    embedGitHub ua comp = do
        token <- ask
        lift $ do 
            lift $ comp (Just ua) token
                    
-- This instance is a bit too literal. Should be possible to do it reursively
instance HasGitHub (a -> Pre b) where
    embedGitHub ua comp arg = embedGitHub ua (comp arg)
instance HasGitHub (a -> b -> Pre c) where
    embedGitHub ua comp arg = embedGitHub ua (comp arg)
instance HasGitHub (a -> b -> c -> Pre d) where
    embedGitHub ua comp arg = embedGitHub ua (comp arg)
instance HasGitHub (a -> Pres b) where
    embedGitHub ua comp arg = embedGitHub ua (comp arg)
instance HasGitHub (a -> b -> Pres c) where
    embedGitHub ua comp arg = embedGitHub ua (comp arg)
instance HasGitHub (a -> b -> c -> Pres d) where
    embedGitHub ua comp arg = embedGitHub ua (comp arg)

github :: (HasClient (AddHeaders api), HasGitHub (Client (AddHeaders api))) 
       => UserAgent -> Proxy api -> GHF (Client (AddHeaders api))
github ua px = embedGitHub ua (clientWithHeaders px)

clientWithHeaders :: HasClient (AddHeaders api) => Proxy api -> Client (AddHeaders api)
clientWithHeaders (Proxy :: Proxy api) = client (Proxy :: Proxy (AddHeaders api)) host 


-- | Pagination options that can be set, including the page number, and the per_page
data Pagination = Pagination { perPage :: Int
                             , page    :: Int
                             , links   :: Maybe [Link]
                             , recurse :: Bool 
                             } deriving Show
defPagination :: Pagination
defPagination = Pagination 30 1 Nothing True

hasNextLink :: Pagination -> Bool
hasNextLink pg = maybe False hnl (links pg)
    where hnl = Prelude.any (\ln -> (Rel, "next") `elem` linkParams ln)
    


