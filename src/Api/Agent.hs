{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Agent
    ( AgentAPI
    , agentAPI
    ) where


import           Config                      (AppM)
-- import           Data.Aeson                  (ToJSON (..), object, (.=))
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (get, toSqlKey)
import           Logger                      (Severity (..), katipAddContext,
                                              katipAddNamespace, logLocM)
import           Models.Agent                (Agent)
import           Persist                     (runDB)
import           Servant                     ((:<|>) ((:<|>)), (:>), Capture,
                                              Get, JSON, Proxy (Proxy), err404,
                                              throwError)

type AgentAPI = "agents" :>
        (    Get '[JSON] [Agent]
        :<|> Capture "id" Int64 :> Get '[JSON] Agent
        )

agentAPI :: AppM [Agent] :<|> (Int64 -> AppM Agent)
agentAPI = agents :<|> agent

agent :: Int64 -> AppM Agent
agent id = do
    katipAddNamespace "agents" $
        -- katipAddContext (AgentLogCTX id) $
        logLocM DebugS "retrieve agent"
    maybeAgent <- runDB (get $ toSqlKey id)
    case maybeAgent of
         Nothing ->
            throwError err404
         Just agent ->
            return agent

-- newtype AgentLogCTX = AgentLogCTX Int64

-- instance ToJSON AgentLogCTX where
--     toJSON (AgentLogCTX id) = object ["agent_id" .= id]

-- instance ToObject AgentLogCTX

-- instance LogItem AgentLogCTX where
--     payloadKeys _verb _a = AllKeys

agents :: AppM [Agent]
agents = do
    katipAddNamespace "agents" $
        logLocM DebugS "requested agents"
    return []
