{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module NRQL where

import           Control.Concurrent             ( threadDelay )
import           Control.Monad.Catch            ( catch
                                                , MonadCatch
                                                , try
                                                , SomeException
                                                )
import           Data.ByteString                ( ByteString )
import           Data.Typeable                  ( typeOf )
import           Data.Either                    ( Either(..) )
import           Data.Aeson                     ( Value
                                                , FromJSON(..)
                                                , ToJSON
                                                , FromJSON
                                                , encode
                                                , decode
                                                , Value(Object, Array)
                                                , (.:)
                                                , (.:?)
                                                )
import           Data.Aeson.Types               ( Parser )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import qualified Data.Vector                   as V
import           Data.List                      ( head )
import           Config                         ( cfgNREndpoint
                                                , AppConfig
                                                )
import           Control.Monad.Reader           ( asks
                                                , MonadReader
                                                )
import           Logger                         ( logLocM
                                                , logStr
                                                , Severity(..)
                                                , KatipContext
                                                )
import           Network.HTTP.Req               ( parseUrlHttp
                                                , HttpResponseBody
                                                , JsonResponse
                                                , MonadHttp
                                                , req
                                                , POST(POST)
                                                , ReqBodyJson(ReqBodyJson)
                                                , jsonResponse
                                                , responseBody
                                                )
import           GHC.Generics                   ( Generic )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )

newtype Account = Account {
        accNumber :: Integer
    } deriving(Generic, Show)

instance ToJSON Account

instance FromJSON Account where
    parseJSON o = do
        r <- responseResults o
        Account <$> foldParsers (accountFromResults r <$> ["min", "max"])

foldParsers :: (FromJSON b) => [Parser (Maybe b)] -> Parser b
foldParsers []       = fail "none of the allowed functions attributes was found"
foldParsers (p : ps) = p >>= maybe (foldParsers ps) return

responseResults :: Value -> Parser (V.Vector Value)
responseResults v = case v of
    Object o -> do
        r <- o .: "results"
        case r of
            Array v -> return v
            _       -> fail "array of results expected"
    _ -> fail "root object expected"

accountFromResults :: (FromJSON b) => V.Vector Value -> Text -> Parser (Maybe b)
accountFromResults v f = case length v of
    0 -> fail "results array from query is empty"
    _ -> case V.head v of
        Object m -> m .:? f
        _        -> fail "array of result objects expected"

type ReqFormat = String

data ArchiveFunction
    = Min
    | Max

data Query = Query Select From Where Since deriving (Show)

type Select = String
type From = String
type Where = String
type Since = String

data DiracRequest = DiracRequest
    { reqQuery :: String
    , reqAccount :: Account
    , reqFormat :: String
    , reqJsonVersion :: Integer
    , reqMetadata :: DiracMetadata
    } deriving(Generic, Show)

instance ToJSON DiracRequest
instance FromJSON DiracRequest

newtype DiracMetadata = DiracMetadata
    { dmSource :: String
    } deriving(Generic, Show)

instance ToJSON DiracMetadata
instance FromJSON DiracMetadata

archiveQuery :: ArchiveFunction -> Query
archiveQuery f =
    let f' = case f of
            Min -> "min(account)"
            Max -> "max(account)"
    in  Query f' "ArchiveFile" "storedEventType = 'SystemSample'" "1 week ago"

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

requestBody :: Query -> DiracRequest
requestBody q = DiracRequest { reqQuery       = encodeQuery q
                             , reqAccount     = Account 313870
                             , reqFormat      = "json"
                             , reqJsonVersion = 1
                             , reqMetadata    = DiracMetadata "NRI-CUSTOMERS"
                             }

request
    :: forall m a b
     . ( KatipContext m
       , MonadReader AppConfig m
       , MonadHttp m
       , MonadCatch m
       , ToJSON a
       , FromJSON b
       )
    => a
    -> m (JsonResponse b)
request body = do
    (url, options) <- asks cfgNREndpoint
    r              <-
        try $ req POST url (ReqBodyJson body) jsonResponse options :: m
            (Either SomeException (JsonResponse b))
    case r of
        Right v -> return v
        Left  e -> do
            liftIO $ print e
            logLocM WarningS ((logStr . show) e)
            liftIO $ threadDelay 5000000
            request body

getAccount
    :: (KatipContext m, MonadCatch m, MonadHttp m, MonadReader AppConfig m)
    => ArchiveFunction
    -> m Account
getAccount f = do
    -- Get the min and max account numbers
    let query = archiveQuery f
        body  = requestBody query
    r <- request body
    let account = responseBody r :: Account
    return account

nrqlPipeline
    :: forall m
     . (KatipContext m, MonadCatch m, MonadHttp m, MonadReader AppConfig m)
    => m ()
nrqlPipeline = do

    a <- getAccount Min
    liftIO $ print a
    liftIO $ threadDelay 1000000

    return ()
