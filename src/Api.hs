{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api
    ( app
    )
where

import           Api.Agent                      ( AgentAPI
                                                , agentAPI
                                                )
import           Api.Dirac                      ( DiracAPI
                                                , diracAPI
                                                )
import           Config                         ( AppConfig
                                                , AppM(runApp)
                                                )
import           Control.Monad.Reader           ( runReaderT )
import           Network.Wai                    ( Application )
import           Servant                        ( Proxy(Proxy)
                                                , (:<|>)((:<|>))
                                                )
import           Servant.Server                 ( Handler(Handler)
                                                , Server
                                                , ServerT
                                                , hoistServer
                                                , serve
                                                )


type AppAPI = AgentAPI :<|> DiracAPI

appServer :: ServerT AppAPI AppM
appServer = agentAPI :<|> diracAPI

hoistedAppServer :: AppConfig -> Server AppAPI
hoistedAppServer cfg = hoistServer appAPI (appToHandler cfg) appServer

appToHandler :: AppConfig -> AppM a -> Handler a
appToHandler cfg appt = flip runReaderT cfg $ runApp appt

appAPI :: Proxy AppAPI
appAPI = Proxy

app :: AppConfig -> Application
app cfg = serve appAPI (hoistedAppServer cfg)
