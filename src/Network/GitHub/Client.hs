{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.GitHub.Client
where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Either
import Data.Proxy
import GHC.TypeLits
import Data.Text

import Servant.API
import Servant.Client

import Network.GitHub.Authentication

host :: BaseUrl
host = BaseUrl Https "api.github.com" 443

type UserAgent = Text
type GitHub = ReaderT (Maybe AuthToken) (EitherT ServantError IO) 

runGitHub :: GitHub a -> Maybe AuthToken -> IO (Either ServantError a)
runGitHub comp = runEitherT . runReaderT comp 

-- Issues that want to support in monad automatically
-- User-Agent, Auth Token, Pagination Headers, Rate limiting 

type family AddHeaders a :: * where
    AddHeaders ((sym :: Symbol) :> last) = (sym :: Symbol) :> AddHeaders last
    AddHeaders (first :> last)           = first :> AddHeaders last
    AddHeaders last                      = Header "User-Agent" Text :> OAuth2Token :> last

type Pre a = Maybe UserAgent -> Maybe AuthToken -> EitherT ServantError IO a

type family GHF a :: * where
    GHF (Pre a) = GitHub a
    GHF (a -> b) = a -> GHF b

-- This instance is a bit too literal. Should be possible to do it reursively
class HasGitHub a where
    embedGitHub :: UserAgent -> a -> GHF a
instance HasGitHub (Pre a) where
    embedGitHub ua comp = ReaderT (comp (Just ua))
instance HasGitHub (a -> Pre b) where
    embedGitHub ua comp arg = embedGitHub ua (comp arg)
instance HasGitHub (a -> b -> Pre c) where
    embedGitHub ua comp arg = embedGitHub ua (comp arg)
instance HasGitHub (a -> b -> c -> Pre d) where
    embedGitHub ua comp arg = embedGitHub ua (comp arg)

github :: (HasClient (AddHeaders api), HasGitHub (Client (AddHeaders api))) 
       => UserAgent -> Proxy api -> GHF (Client (AddHeaders api))
github ua px = embedGitHub ua (clientWithHeaders px)

clientWithHeaders :: HasClient (AddHeaders api) => Proxy api -> Client (AddHeaders api)
clientWithHeaders (Proxy :: Proxy api) = client (Proxy :: Proxy (AddHeaders api)) host 




