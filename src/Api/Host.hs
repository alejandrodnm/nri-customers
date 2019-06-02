{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Host
    ( HostAPI
    , hostAPI
    )
where

import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Reader           ( asks )
import           Data.Int                       ( Int64 )
import           Logger                         ( Severity(..)
                                                , katipAddContext
                                                , katipAddNamespace
                                                , logLocM
                                                )
import           Models.Host                    ( Host
                                                , EntityField(HostEntityId)
                                                )
import           Servant                        ( (:<|>)((:<|>))
                                                , (:>)
                                                , Capture
                                                , Get
                                                , JSON
                                                , Proxy(Proxy)
                                                , err404
                                                , throwError
                                                )

import           Config                         ( AppM
                                                , AppConfig(cfgRepoInterpreter)
                                                )
import           Repo                           ( retrieveHosts )

type HostAPI = "hosts" :> Get '[JSON] [Host]

hostAPI :: AppM [Host]
hostAPI = hosts

hosts :: AppM [Host]
hosts = do
    logLocM DebugS "requested hosts"
    repoInterpreter <- asks cfgRepoInterpreter
    repoInterpreter retrieveHosts
