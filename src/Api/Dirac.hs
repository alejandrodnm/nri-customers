{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Dirac
    ( DiracAPI
    , diracAPI
    )
where


import           Config                         ( AppM )
import           Data.Aeson
import           Data.Int                       ( Int64 )
import           Data.List                      ( intersect )
import           Data.Maybe                     ( fromJust )
import           Data.Text                      ( Text
                                                , isInfixOf
                                                , pack
                                                )
import           Database.Persist.Postgresql    ( get
                                                , toSqlKey
                                                )
import           GHC.Generics                   ( Generic )
import           Logger                         ( Severity(..)
                                                , katipAddContext
                                                , katipAddNamespace
                                                , logLocM
                                                , logStr
                                                )
import           Types.Account                  ( Account )
import           NRQL.Client                    ( DiracMetadata )
import           Servant                        ( (:<|>)((:<|>))
                                                , (:>)
                                                , Capture
                                                , Get
                                                , JSON
                                                , Post
                                                , Proxy(Proxy)
                                                , ReqBody
                                                , err404
                                                , throwError
                                                )

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
    | "latest(" `isInfixOf` query
    = fromJust
        $ decode
              "{\"facets\":[{\"name\":\"974205676942181148\",\"results\":[{\"latest\":\"CentOSLinux7(Core)\"},{\"latest\":\"1.3.20\"},{\"latest\":\"3.10.0-514.21.1.el7.x86_64\"},{\"latest\":\"unknown\"},{\"latest\":\"linux\"},{\"latest\":null},{\"latest\":null},{\"latest\":null},{\"latest\":\"14\"},{\"latest\":\"56\"},{\"latest\":\"269033377792\"}]},{\"name\":\"8923018336323236291\",\"results\":[{\"latest\":\"ContainerLinuxbyCoreOS1911.5.0(Rhyolite)\"},{\"latest\":\"1.0.1019\"},{\"latest\":\"4.14.84-coreos\"},{\"latest\":\"SupermicroSYS-6019U-TN4R4T\"},{\"latest\":\"linux\"},{\"latest\":null},{\"latest\":null},{\"latest\":null},{\"latest\":\"18\"},{\"latest\":\"72\"},{\"latest\":\"404430925824\"}]}]}" :: Value
    | otherwise
    = error "wrong query"
