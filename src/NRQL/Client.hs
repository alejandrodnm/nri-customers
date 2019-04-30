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
import           NRQL.Query                     ( Query
                                                , encodeQuery
                                                )
import           Types.Account                  ( Account(..)
                                                , Accounts
                                                )
import           Types.Host                     ( Host(..)
                                                , Hosts(..)
                                                , HostsCount(..)
                                                )

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

runQuery
    :: ( KatipContext m
       , MonadCatch m
       , MonadHttp m
       , MonadReader AppConfig m
       , FromJSON a
       )
    => Account
    -> Query
    -> (JsonResponse a -> a)
    -> m a
runQuery acc query f = do
    let body = requestBody query acc
    r <- request body
    return $ f r
