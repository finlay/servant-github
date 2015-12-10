{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Network.GitHub.Client
where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Either
import Data.Proxy
import Data.Text
import GHC.TypeLits

import Servant.API
import Servant.Client

import Network.GitHub.Authentication

host :: BaseUrl
host = BaseUrl Https "api.github.com" 443

type UserAgent = Text
useragent :: UserAgent
useragent = "servant-github"

type ServantClient = EitherT ServantError IO
type GitHub = ReaderT (Maybe AuthToken) (EitherT ServantError IO) 
runGitHub :: GitHub a -> Maybe AuthToken -> IO (Either ServantError a)
runGitHub comp = runEitherT . runReaderT comp 

class HasGitHub layout where
    type GH layout :: *
    github :: UserAgent -> Proxy layout -> GH layout

instance ( HasClient (Get ct a) 
         , Client (Get ct a) ~ EitherT ServantError IO a )
        => HasGitHub (Get ct a) where
    type GH (Get ct a) = GitHub a
    github ua (Proxy :: Proxy (Get ct a)) = 
        let layout :: Proxy (Header "User-Agent" Text :> OAuth2Token :> (Get ct a))
            layout = Proxy
        in  ReaderT (client layout host (Just ua))

instance ( KnownSymbol path, HasGitHub layout )
        => HasGitHub (path :> layout) where
    type GH (path :> layout) = GH layout
    github ua (Proxy :: Proxy (path :> layout)) 
        = github ua (Proxy :: Proxy (path :> layout))

-- instance ( HasGitHub layout, HasClient (arg :> layout)
--          , Client (arg :> layout) ~ (a -> Client layout) )
--         => HasGitHub (arg :> layout) where
--     type GH (arg :> layout) = a -> GH layout
--     github ua (Proxy :: Proxy (arg :> layout)) val
--         = github ua (Proxy :: Proxy (arg :> layout)) <*> return val




-- class Enter typ arg ret | typ arg -> ret, typ ret -> arg where
--     enter :: arg -> typ -> ret
-- 
-- instance (Enter (ServantClient b) arg (GitHub b) 
--     => Enter (a -> ServantClient b) arg (a -> GitHub b) where
--     enter arg f a = enter arg (f a)
