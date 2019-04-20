{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NRQL where

import           Config                 (AppConfig, cfgNREndpoint)
import           Control.Concurrent     (threadDelay)
import           Control.Monad.Catch    (MonadCatch, SomeException, catch, try)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, asks)
import           Data.Aeson             (FromJSON (..), ToJSON, Value,
                                         Value (Array, Object), decode, encode,
                                         (.:), (.:?))
import           Data.Aeson.Types       (Parser)
import           Data.ByteString        (ByteString)
import           Data.Either            (Either (..))
import           Data.List              (head)
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import           Data.Typeable          (typeOf)
import qualified Data.Vector            as V
import           GHC.Generics           (Generic)
import           Logger                 (KatipContext, Severity (..), logLocM,
                                         logStr)
import           Network.HTTP.Req       (HttpResponseBody, JsonResponse,
                                         MonadHttp, POST (POST),
                                         ReqBodyJson (ReqBodyJson),
                                         jsonResponse, parseUrlHttp, req,
                                         responseBody)

newtype Account = Account {
        accNumber :: Integer
    } deriving(Generic, Show)

instance ToJSON Account

instance FromJSON Account where
    parseJSON o = do
        r <- responseResults o
        Account <$> foldParsers (accountFromResults r <$> ["min", "max"])

-- Returns the first valid parser, is no parser is valid, it fails.
foldParsers :: (FromJSON b) => [Parser (Maybe b)] -> Parser b
foldParsers []       = fail "none of the allowed functions attributes was found"
foldParsers (p : ps) = p >>= maybe (foldParsers ps) return

-- Returns a parser up to the `results` attribute of the response
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
    | Uniques

data Query = Query Select From Where Since deriving (Show)

type Select = String
type From = String
type Where = String
type Since = String

data DiracRequest = DiracRequest
    { reqQuery       :: String
    , reqAccount     :: Account
    , reqFormat      :: String
    , reqJsonVersion :: Integer
    , reqMetadata    :: DiracMetadata
    } deriving(Generic, Show)

instance ToJSON DiracRequest
instance FromJSON DiracRequest

newtype DiracMetadata = DiracMetadata
    { dmSource :: String
    } deriving(Generic, Show)

instance ToJSON DiracMetadata
instance FromJSON DiracMetadata

retrieveAccountQuery :: ArchiveFunction -> Query
retrieveAccountQuery f =
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

-- Retrieves the account resulting from applying the given function
getAccount
    :: (KatipContext m, MonadCatch m, MonadHttp m, MonadReader AppConfig m)
    => ArchiveFunction
    -> m Account
getAccount f = do
    -- Get the min and max account numbers
    let query = retrieveAccountQuery f
        body  = requestBody query
    r <- request body
    let account = responseBody r :: Account
    return account
