{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Dirac
    ( DiracAPI
    , diracAPI
    )
where


import           Config                         ( AppM )
import           Data.Int                       ( Int64 )
import           Database.Persist.Postgresql    ( get
                                                , toSqlKey
                                                )
import           Logger                         ( Severity(..)
                                                , katipAddContext
                                                , katipAddNamespace
                                                , logLocM
                                                , logStr
                                                )
import           Servant                        ( (:<|>)((:<|>))
                                                , (:>)
                                                , Capture
                                                , Get
                                                , JSON
                                                , Proxy(Proxy)
                                                , err404
                                                , throwError
                                                , Post
                                                , ReqBody
                                                )
import           Data.Text                      ( Text
                                                , isInfixOf
                                                , pack
                                                )
import           NRQL
import           Data.List                      ( intersect )
import           Data.Aeson
import           GHC.Generics                   ( Generic )
import           Data.Maybe                     ( fromJust )

type DiracAPI = "dirac" :> ReqBody '[JSON] DiracRequest :> Post '[JSON] Value

diracAPI :: DiracRequest -> AppM Value
diracAPI = dirac

dirac :: DiracRequest -> AppM Value
dirac r = do
    katipAddNamespace "diracs" $ logLocM DebugS (logStr $ reqQuery r)
    return $ getDiracRestul' (pack $ reqQuery r)

getDiracRestul' :: Text -> Value
getDiracRestul' query
    | "min(" `isInfixOf` query
    = fromJust $ decode "{\"results\":[{\"min\":4.0}]}" :: Value
    | "max(" `isInfixOf` query
    = fromJust $ decode "{\"results\":[{\"max\":4.0}]}" :: Value
    | otherwise
    = error "wrong query"
