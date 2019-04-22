{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Dirac
    ( DiracAPI
    , diracAPI
    )
where


import           Config                      (AppM)
import           Data.Aeson
import           Data.Int                    (Int64)
import           Data.List                   (intersect)
import           Data.Maybe                  (fromJust)
import           Data.Text                   (Text, isInfixOf, pack)
import           Database.Persist.Postgresql (get, toSqlKey)
import           GHC.Generics                (Generic)
import           Logger                      (Severity (..), katipAddContext,
                                              katipAddNamespace, logLocM,
                                              logStr)
import           Models.Account              (Account)
import           NRQL                        (DiracMetadata)
import           Servant                     ((:<|>) ((:<|>)), (:>), Capture,
                                              Get, JSON, Post, Proxy (Proxy),
                                              ReqBody, err404, throwError)

data DiracRequest = DiracRequest
    { query       :: String
    , account     :: Integer
    , format      :: String
    , jsonVersion :: Integer
    , metadata    :: DiracMetadata
    } deriving(Generic, Show)

instance FromJSON DiracRequest

type DiracAPI = "dirac" :> ReqBody '[JSON] DiracRequest :> Post '[JSON] Value

diracAPI :: DiracRequest -> AppM Value
diracAPI = dirac

dirac :: DiracRequest -> AppM Value
dirac r = do
    katipAddNamespace "diracs" $ logLocM DebugS (logStr $ query r)
    return $ getDiracRestul' (pack $ query r)

getDiracRestul' :: Text -> Value
getDiracRestul' query
    | "min(account)" `isInfixOf` query
    = fromJust $ decode "{\"results\":[{\"min\":24.0}]}" :: Value
    | "max(account)" `isInfixOf` query
    = fromJust $ decode "{\"results\":[{\"max\":42.0}]}" :: Value
    | "uniques(account)" `isInfixOf` query
    = fromJust $ decode "{\"results\":[{\"members\":[24.0, 42.0]}]}" :: Value
    | "uniqueCount(entityId)" `isInfixOf` query
    = fromJust $ decode "{\"results\":[{\"uniqueCount\":5}]}" :: Value
    | otherwise
    = error "wrong query"
