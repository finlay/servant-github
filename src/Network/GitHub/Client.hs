{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{--# LANGUAGE ExistentialQuantification #-}
{--# LANGUAGE AllowAmbiguousTypes #-}
module Network.GitHub.Client
where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Either
import Data.Proxy
import Data.Typeable
import GHC.TypeLits
import Data.Text

import Servant.API
import Servant.Client

import Network.GitHub.Authentication

host :: BaseUrl
host = BaseUrl Https "api.github.com" 443

type UserAgent = Text
useragent :: UserAgent
useragent = "servant-github"

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


--github3 :: UserAgent -> a -> GHF a
--github3 ua (comp :: Pre a) = ReaderT (comp (Just ua))
--github3 ua (Wrap (comp :: a -> b)) = \github3 ua 

class HasGitHub a where
    github3 :: UserAgent -> a -> GHF a
instance HasGitHub (Pre a) where
    github3 ua comp = ReaderT (comp (Just ua))
-- instance HasGitHub b => HasGitHub (a -> b) where
--     github3 ua comp arg = github3 ua (comp arg)
-- 
github = undefined

github2 :: HasClient (AddHeaders api) => Proxy api -> Client (AddHeaders api)
github2 (Proxy :: Proxy api) = client (Proxy :: Proxy (AddHeaders api)) host 


class Enter typ arg ret | typ arg -> ret, typ ret -> arg where
    enter :: arg -> typ -> ret

-- **  Servant combinators
instance ( Enter typ1 arg1 ret1, Enter typ2 arg2 ret2
         , arg1 ~ arg2
         ) => Enter (typ1 :<|> typ2) arg1 (ret1 :<|> ret2) where
    enter e (a :<|> b) = enter e a :<|> enter e b

instance (Enter b arg ret) => Enter (a -> b) arg (a -> ret) where
    enter arg f a = enter arg (f a)

-- ** Useful instances

-- | A natural transformation from @m@ to @n@. Used to `enter` particular
-- datatypes.
newtype m :~> n = Nat { unNat :: forall a. m a -> n a} deriving Typeable

instance Enter (m a) (m :~> n) (n a) where
    enter (Nat f) = f

-- data AddHeaders a = AddHeaders 
-- 
-- instance ( HasClient (first :> last), HasClient last )
--         => HasClient (AddHeaders (first :> last)) where
--     type Client (AddHeaders (first :> last)) = Client (first :> AddHeaders last)
--     clientWithRoute (Proxy :: Proxy (AddHeaders (first :> last)))
--         = clientWithRoute (Proxy :: Proxy (first :> AddHeaders last)) 
-- 
-- instance ( HasClient layout, Client layout ~ EitherT ServantError IO a)
--          => HasClient (AddHeaders layout) where
--     type Client (AddHeaders layout) 
--         = Client (Header "User-Agent" Text :> OAuth2Token :> layout)
--     clientWithRoute (Proxy :: Proxy (AddHeaders layout))
--         = clientWithRoute (Proxy :: Proxy (Header "User-Agent" Text :> OAuth2Token :> layout))
-- 
-- type family EmbedGitHub t :: *
-- type instance EmbedGitHub (EitherT ServantError IO a) 
--     = Maybe AuthToken -> EitherT ServantError IO a
-- type instance EmbedGitHub (a -> b) = a -> EmbedGitHub b
-- 
-- --github2 :: (HasClient api, HasClient (AddHeaders api))
-- --        => Proxy api -> Client (AddHeaders api)
-- github2 (Proxy :: Proxy api) = 
--     client (Proxy :: Proxy (AddHeaders api)) host 

-- class HasGitHub layout where
--     type GH layout :: *
--     github :: UserAgent -> Proxy layout -> GH layout
-- 
-- instance ( HasClient (Get ct a) 
--          , Client (Get ct a) ~ EitherT ServantError IO a )
--         => HasGitHub (Get ct a) where
--     type GH (Get ct a) = GitHub a
--     github ua (Proxy :: Proxy (Get ct a)) = 
--         let layout :: Proxy (AddHeaders (Get ct a))
--             layout = Proxy
--         in  ReaderT (client layout host (Just ua))
-- 
-- instance ( KnownSymbol path, HasGitHub layout )
--         => HasGitHub (path :> layout) where
--     type GH (path :> layout) = GH layout
--     github ua (Proxy :: Proxy (path :> layout)) 
--         = github ua (Proxy :: Proxy (path :> layout))

-- instance ( HasGitHub layout, HasClient (arg :> layout)
--          , Client (arg :> layout) ~ (a -> Client layout) )
--         => HasGitHub (arg :> layout) where
--     type GH (arg :> layout) = a -> GH layout
--     github ua (Proxy :: Proxy (arg :> layout)) val
--         = github ua (Proxy :: Proxy (arg :> layout)) <*> return val



