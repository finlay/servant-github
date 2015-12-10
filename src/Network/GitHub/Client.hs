{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Network.GitHub.Client
where

import Control.Monad.Trans.Class
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

type UserAgent = Maybe Text
useragent :: UserAgent
useragent = Just "servant-github"

addHeaders :: HasClient layout 
           => BaseUrl -> Proxy layout -> Maybe Text -> Maybe AuthToken 
           -> Client layout
addHeaders host (Proxy :: Proxy layout) =
    let layout :: Proxy (Header "User-Agent" Text :> OAuth2Token :> layout)
        layout = Proxy
    in  client layout host

type GitHub = ReaderT (Maybe AuthToken) (EitherT ServantError IO) 
runGitHub :: GitHub a -> Maybe AuthToken -> IO (Either ServantError a)
runGitHub comp = runEitherT . runReaderT comp 

single :: (Monad m, HasClient layout, Client layout ~ m b) =>
     UserAgent -> Proxy layout -> ReaderT (Maybe AuthToken) m b
single ua px = do
    token <- ask
    lift $ addHeaders host px ua token

witharg :: (Monad m, HasClient layout, Client layout ~ (a -> m b)) =>
     UserAgent -> Data.Proxy.Proxy layout -> a -> ReaderT (Maybe AuthToken) m b
witharg ua px arg = do
    token <- ask
    lift $ addHeaders host px ua token arg
    
class HasGitHub layout where
    type GH layout :: *
    github :: UserAgent -> Proxy layout -> GH layout 

instance (HasGitHub a, HasGitHub b) => HasGitHub (a :<|> b) where
  type GH (a :<|> b) = GH a :<|> GH b
  github ua Proxy = github ua (Proxy :: Proxy a) :<|>
                    github ua (Proxy :: Proxy b) 

instance ( KnownSymbol capture, ToText a, HasGitHub sublayout )
      => HasGitHub (Capture capture a :> sublayout) where
    type GH (Capture capture a :> sublayout) = a -> GH sublayout
    github ua px val = witharg ua px val

instance ( HasClient layout, layout ~ (Get cts a)
         , Client layout ~ EitherT ServantError IO a)
    => HasGitHub (Get cts a) where
    type GH (Get cts a) = GitHub a
    github ua px = single ua px

instance ( HasClient layout, layout ~ (Post cts a)
         , Client layout ~ EitherT ServantError IO a)
    => HasGitHub (Post cts a) where
    type GH (Post cts a) = GitHub a
    github ua px = single ua px

instance ( HasClient layout, layout ~ (Put cts a)
         , Client layout ~ EitherT ServantError IO a)
    => HasGitHub (Put cts a) where
    type GH (Put cts a) = GitHub a
    github ua px = single ua px

instance ( HasClient layout, layout ~ (Delete cts a)
         , Client layout ~ EitherT ServantError IO a)
    => HasGitHub (Delete cts a) where
    type GH (Delete cts a) = GitHub a
    github ua px = single ua px

instance ( HasClient layout, layout ~ (Patch cts a)
         , Client layout ~ EitherT ServantError IO a)
    => HasGitHub (Patch cts a) where
    type GH (Patch cts a) = GitHub a
    github ua px = single ua px


--instance (KnownSymbol sym, ToText a, HasGitHub sublayout)
--      => HasGitHub (Header sym a :> sublayout) where
--  type GH (Header sym a :> sublayout) =
--    Maybe a -> GH sublayout
--  github Proxy  mval =
--
--instance (KnownSymbol sym, ToText a, HasGitHub sublayout)
--      => HasGitHub (QueryParam sym a :> sublayout) where
--  type GH (QueryParam sym a :> sublayout) =
--    Maybe a -> GH sublayout
--  github Proxy mparam =
--
--instance (KnownSymbol sym, ToText a, HasGitHub sublayout)
--      => HasGitHub (QueryParams sym a :> sublayout) where
--  type GH (QueryParams sym a :> sublayout) =
--    [a] -> GH sublayout
--  github paramlist =
--
--instance (KnownSymbol sym, HasGitHub sublayout)
--      => HasGitHub (QueryFlag sym :> sublayout) where
--  type GH (QueryFlag sym :> sublayout) =
--    Bool -> GH sublayout
--  github Proxy flag =
--
--instance (KnownSymbol sym, ToText a, HasGitHub sublayout)
--      => HasGitHub (MatrixParam sym a :> sublayout) where
--  type GH (MatrixParam sym a :> sublayout) =
--    Maybe a -> GH sublayout
--  github Proxy mparam =
--
--instance (KnownSymbol sym, ToText a, HasGitHub sublayout)
--      => HasGitHub (MatrixParams sym a :> sublayout) where
--  type GH (MatrixParams sym a :> sublayout) =
--    [a] -> GH sublayout
--  github Proxy paramlist =
--
--instance (KnownSymbol sym, HasGitHub sublayout)
--      => HasGitHub (MatrixFlag sym :> sublayout) where
--  type GH (MatrixFlag sym :> sublayout) =
--    Bool -> GH sublayout
--  github Proxy flag =
--
--instance (MimeRender ct a, HasGitHub sublayout)
--      => HasGitHub (ReqBody (ct ': cts) a :> sublayout) where
--  type GH (ReqBody (ct ': cts) a :> sublayout) =
--    a -> GH sublayout
--  github Proxy body =
--
instance (KnownSymbol path, HasGitHub sublayout) => HasGitHub (path :> sublayout) where
  type GH (path :> sublayout) = GH sublayout
  github ua Proxy = github ua (Proxy :: Proxy sublayout)

