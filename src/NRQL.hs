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

type Account = Integer

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
    { query :: String
    , account :: Account
    , format :: String
    , jsonVersion :: Integer
    , metadata :: DiracMetadata
    } deriving(Generic, Show)

instance ToJSON DiracRequest
instance FromJSON DiracRequest

newtype DiracMetadata = DiracMetadata
    { source :: String
    } deriving(Generic, Show)

instance ToJSON DiracMetadata
instance FromJSON DiracMetadata

newtype MinResult = MinResult {
    min :: Double
    } deriving (Show, Generic)

instance FromJSON MinResult where
    parseJSON o = do
        r <- diractResponseResults o
        MinResult <$> attributeFromResults r "min"

newtype MaxResult = MaxResult {
    max :: Double
    } deriving (Show, Generic)

instance FromJSON MaxResult where
    parseJSON o = do
        r <- diractResponseResults o
        MaxResult <$> attributeFromResults r "max"

diractResponseResults :: Value -> Parser (V.Vector Value)
diractResponseResults v = case v of
    Object o -> do
        r <- o .: "results"
        case r of
            Array v -> return v
            _       -> fail "array of results expected"
    _ -> fail "object expected"

attributeFromResults :: (FromJSON b) => V.Vector Value -> Text -> Parser b
attributeFromResults v f = case length v of
    0 -> fail "results array from query is empty"
    _ -> case V.head v of
        Object m -> m .: f
        _        -> fail "attribute from result not found"

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
requestBody q = DiracRequest { query       = encodeQuery q
                             , account     = 313870
                             , format      = "json"
                             , jsonVersion = 1
                             , metadata    = DiracMetadata "NRI-CUSTOMERS"
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

nrqlPipeline
    :: (KatipContext m, MonadCatch m, MonadHttp m, MonadReader AppConfig m)
    => m ()
nrqlPipeline = do
    -- Get the min and max account numbers
    let queryMin     = archiveQuery Min
        queryMinbody = requestBody queryMin
    minR <- request queryMinbody
    let minAcc = responseBody minR :: MinResult
    liftIO $ threadDelay 1000000

    let queryMax     = archiveQuery Max
        queryMaxbody = requestBody queryMax
    maxR <- request queryMaxbody
    let maxAcc = responseBody maxR :: MaxResult
    liftIO $ threadDelay 1000000

    return ()
