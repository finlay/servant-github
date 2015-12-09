{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
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

useragent :: Maybe Text
useragent = Just "servant-github"

type GitHub = ReaderT (Maybe AuthToken) (EitherT ServantError IO) 

runGitHub :: GitHub a -> Maybe AuthToken -> IO (Either ServantError a)
runGitHub comp = runEitherT . runReaderT comp 

class HasGitHub layout where
    type GH layout :: *
    github :: Proxy layout -> GH layout 

instance (HasGitHub a, HasGitHub b) => HasGitHub (a :<|> b) where
  type GH (a :<|> b) = GH a :<|> GH b
  github Proxy = github (Proxy :: Proxy a) :<|>
                 github (Proxy :: Proxy b) 

--instance (KnownSymbol capture, ToText a, HasGitHub sublayout)
--      => HasGitHub (Capture capture a :> sublayout) where
--  type GH (Capture capture a :> sublayout) = a -> GH sublayout
--  github Proxy val = github (Proxy :: Proxy sublayout)

instance (HasClient layout, layout ~ (Delete cts' a))
  => HasGitHub (Delete cts' a) where
  type GH (Delete cts' a) = GitHub a
  github Proxy = do
      token <- ask
      lift $ call useragent token
    where
      layout :: Proxy (Header "User-Agent" Text :> OAuth2Token :> layout)
      layout = Proxy
      call :: Maybe Text -> Maybe AuthToken -> Client layout
      call = client layout host

instance ( HasClient layout 
         , layout ~ (Get cts result)
         , Client layout ~ EitherT ServantError IO result)
  => HasGitHub (Get cts result) where
  type GH (Get cts result) = GitHub result
  github Proxy = do
      token <- ask
      lift $ call useragent token
    where
      layout :: Proxy (Header "User-Agent" Text :> OAuth2Token :> layout)
      layout = Proxy
      call :: Maybe Text -> Maybe AuthToken -> Client layout
      call = client layout host


--instance (KnownSymbol sym, ToText a, HasGitHub sublayout)
--      => HasGitHub (Header sym a :> sublayout) where
--  type GH (Header sym a :> sublayout) =
--    Maybe a -> GH sublayout
--  github Proxy  mval =
--
--instance HasGitHub (Post (ct ': cts) a) where
--  type GH (Post (ct ': cts) a) = GitHub a
--  github Proxy  =
--
--instance HasGitHub (Put (ct ': cts) a) where
--  type GH (Put (ct ': cts) a) = GitHub a
--  github Proxy  =
--
--instance HasGitHub (Patch (ct ': cts) a) where
--  type GH (Patch (ct ': cts) a) = GitHub a
--  github Proxy  =
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
  github Proxy = github (Proxy :: Proxy sublayout)

