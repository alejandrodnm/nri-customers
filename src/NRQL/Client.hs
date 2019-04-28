{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NRQL.Client where

import           Control.Concurrent             ( threadDelay )
import           Control.Monad.Catch            ( MonadCatch
                                                , SomeException
                                                , catch
                                                , try
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Reader           ( MonadReader
                                                , asks
                                                )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , encode
                                                )
import           Data.Either                    ( Either(..) )
import           Data.List                      ( head )
import           Data.Maybe                     ( fromMaybe )
import           Data.Typeable                  ( typeOf )
import qualified Data.Vector                   as V
import           GHC.Generics                   ( Generic )
import           Network.HTTP.Req               ( HttpResponseBody
                                                , JsonResponse
                                                , MonadHttp
                                                , POST(POST)
                                                , ReqBodyJson(ReqBodyJson)
                                                , jsonResponse
                                                , toVanillaResponse
                                                , parseUrlHttp
                                                , req
                                                , responseBody
                                                )

import           Config                         ( AppConfig
                                                , cfgNREndpoint
                                                )
import           Logger                         ( KatipContext
                                                , Severity(..)
                                                , logLocM
                                                , logStr
                                                )
import           Types.Account                  ( Account(..)
                                                , Accounts
                                                )
import           Types.Host                     ( Host(..)
                                                , Hosts(..)
                                                , HostsCount(..)
                                                )

nrqlLimit :: Int
nrqlLimit = 1000

type ReqFormat = String

data ArchiveFunction
    = Min
    | Max
    | Uniques

data Query = Query Select From Where Since deriving (Show)

type Select = String
type From = String
type Where = String
type Since = String

data DiracRequest = DiracRequest
    { query       :: String
    , account     :: Account
    , format      :: String
    , jsonVersion :: Integer
    , metadata    :: DiracMetadata
    } deriving(Generic, Show)

instance ToJSON DiracRequest

newtype DiracMetadata = DiracMetadata
    { source :: String
    } deriving(Generic, Show)

instance ToJSON DiracMetadata
instance FromJSON DiracMetadata

accountQuery :: ArchiveFunction -> Query
accountQuery f =
    let f' = case f of
            Min -> "min(account)"
            Max -> "max(account)"
    in  Query f' "ArchiveFile" "storedEventType = 'SystemSample'" "1 week ago"

accountsQuery :: Account -> Account -> Query
accountsQuery bottom top =
    let where' =
                "storedEventType = 'SystemSample' AND account >= "
                    ++ (show . accNumber) bottom
                    ++ " AND account < "
                    ++ (show . accNumber) top
    in  Query "uniques(account)" "ArchiveFile" where' "1 week ago"

hostsCountQuery :: Query
hostsCountQuery =
    Query "uniqueCount(entityId)" "SystemSample" "true" "1 week ago"

hostsCountQueryFilteredByEntity :: String -> Query
hostsCountQueryFilteredByEntity entityId =
    let where' = "entityId like '" ++ entityId ++ "%'"
    in  Query "uniqueCount(entityId)" "SystemSample" where' "1 week ago"

hostsQuery :: Query
hostsQuery =
    let
        select
            = "latest(linuxDistribution), latest(agentVersion), latest(kernelVersion), latest(instanceType), latest(operatingSystem), latest(windowsVersion), latest(windowsPlatform), latest(windowsFamily), latest(coreCount), latest(processorCount), latest(systemMemoryBytes)"
    in  Query select "SystemSample" "true facet entityId" "1 week ago"

hostsQueryFilteredByEntity :: String -> Query
hostsQueryFilteredByEntity entityId =
    let
        select
            = "latest(linuxDistribution), latest(agentVersion), latest(kernelVersion), latest(instanceType), latest(operatingSystem), latest(windowsVersion), latest(windowsPlatform), latest(windowsFamily), latest(coreCount), latest(processorCount), latest(systemMemoryBytes)"
        where' = "entityId like '" ++ entityId ++ "%' facet entityId"
    in
        Query select "SystemSample" where' "1 week ago"

encodeQuery :: Query -> String
encodeQuery (Query select from where' since) =
    "select "
        ++ select
        ++ " from "
        ++ from
        ++ " where "
        ++ where'
        ++ " since "
        ++ since
        ++ " limit "
        ++ show nrqlLimit

metaAccount :: Account
metaAccount = Account 313870

requestBody :: Query -> Account -> DiracRequest
requestBody q acc = DiracRequest { query       = encodeQuery q
                                 , account     = acc
                                 , format      = "json"
                                 , jsonVersion = 1
                                 , metadata    = DiracMetadata "NRI-CUSTOMERS"
                                 }

-- Executes a request with the given `DiracRequest`, in case of and
-- error the request will be retried.
request
    :: forall m b
     . ( KatipContext m
       , MonadReader AppConfig m
       , MonadHttp m
       , MonadCatch m
       , FromJSON b
       )
    => DiracRequest
    -> m (JsonResponse b)
request body = do
    logLocM DebugS ((logStr . show . encode) body)
    (url, options) <- asks cfgNREndpoint
    r              <-
        try $ req POST url (ReqBodyJson body) jsonResponse options :: m
            (Either SomeException (JsonResponse b))
    case r of
        Right v -> return v
        Left  e -> do
            liftIO $ print e
            logLocM ErrorS ((logStr . show) e)
            liftIO $ threadDelay 5000000
            request body

-- Retrieves the accounts resulting from applying the given function
getAccounts
    :: (KatipContext m, MonadCatch m, MonadHttp m, MonadReader AppConfig m)
    => Account
    -> Account
    -> m Accounts
getAccounts bottom top = do
    -- Get the min and max account numbers
    let query = accountsQuery bottom top
        body  = requestBody query metaAccount
    r <- request body
    let accounts = responseBody r :: Accounts
    return accounts

-- Retrieves the account resulting from applying the given function
getAccount
    :: (KatipContext m, MonadCatch m, MonadHttp m, MonadReader AppConfig m)
    => ArchiveFunction
    -> m Account
getAccount f = do
    -- Get the min and max account numbers
    let query = accountQuery f
        body  = requestBody query metaAccount
    r <- request body
    let account = responseBody r :: Account
    return account

getHostsCount
    :: (KatipContext m, MonadCatch m, MonadHttp m, MonadReader AppConfig m)
    => Account
    -> Query
    -> m Int
getHostsCount acc query = do
    let body = requestBody query acc
    r <- request body
    let hostsCount = hCount (responseBody r :: HostsCount)
    return hostsCount

getHosts
    :: (KatipContext m, MonadCatch m, MonadHttp m, MonadReader AppConfig m)
    => Account
    -> Query
    -> m Hosts
getHosts acc query = do
    let body = requestBody query acc
    r <- request body
    let hosts = responseBody r :: Hosts
    return hosts
