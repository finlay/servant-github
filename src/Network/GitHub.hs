{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.GitHub 
    ( module Network.GitHub
    , module Network.GitHub.Authentication
    , module Network.GitHub.Organisation
    )
where

--import Control.Applicative
--import Control.Monad
--import Control.Monad.IO.Class
import Control.Monad.Trans.Either
--import Data.Aeson
import Data.Proxy
import Data.Text
--import GHC.Generics
import Servant.API
import Servant.Client

import Network.GitHub.Authentication
import Network.GitHub.Organisation

host :: BaseUrl
host = BaseUrl Https "api.github.com" 443

type UserOrgs = "user" :> "orgs" :> Get '[JSON] [Organisation]
type OrgTeams = "orgs" :> Capture "org" OrgLogin 
                       :> "teams" :> Get '[JSON] [Team]

type UserAgent = Text
type family WithHeaders a 
type instance WithHeaders a = Header "User-Agent" UserAgent :> OAuth2Token :> a

type family HeaderM a 
type instance HeaderM a = Maybe UserAgent -> Maybe Token -> a

type GitHub = ReaderT Token (EitherT ServantError IO)

runGitHub :: Token -> GitHub a -> IO (Either ServantError a)
runGitHub token comp = runEitherT (runReaderT Token comp)

getOrgs :: HeaderM (EitherT ServantError IO [Organisation])
getOrgs = client (Proxy :: Proxy (WithHeaders UserOrgs)) host

orgTeams :: HeaderM (OrgLogin -> EitherT ServantError IO [Team])
orgTeams = client (Proxy :: Proxy (WithHeaders OrgTeams)) host


Maybe UserAgent -> EitherT ServantError IO a 
~>
ReaderT UserAgent (EitherT ServantError IO) a
