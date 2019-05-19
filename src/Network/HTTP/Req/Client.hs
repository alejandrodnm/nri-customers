{-# LANGUAGE ScopedTypeVariables #-}

module Network.HTTP.Req.Client where

import           Control.Concurrent             ( threadDelay )
import           Control.Monad.Catch            ( MonadCatch
                                                , SomeException
                                                , catch
                                                , try
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , encode
                                                )
import           Data.Proxy
import           Network.HTTP.Req               ( JsonResponse
                                                , MonadHttp
                                                , POST(POST)
                                                , ReqBodyJson(ReqBodyJson)
                                                , Url
                                                , Option
                                                , jsonResponse
                                                , req
                                                , responseBody
                                                , HttpResponseBody
                                                )

import           Logger                         ( KatipContext
                                                , Severity(..)
                                                , logLocM
                                                , logStr
                                                )

class (KatipContext m, MonadHttp m, MonadCatch m, MonadIO m ) => (ReqClient m) where
    reqRequest :: (ToJSON a, FromJSON response) => Proxy response -> Url scheme -> Option scheme -> a -> m response
    reqRequest = request

-- Executes a request with the given `DiracRequest`, in case of an error
-- the request will be retried.
request
    :: forall a m b scheme response
     . ( KatipContext m
       , MonadHttp m
       , MonadCatch m
       , FromJSON response
       , ToJSON a
       )
    => Proxy response
    -> Url scheme
    -> Option scheme
    -> a
    -> m response
request proxy url options body = do
    logLocM DebugS ((logStr . show . encode) body)
    r <-
        try $ req POST url (ReqBodyJson body) jsonResponse options :: m
            (Either SomeException (JsonResponse response))
    case r of
        Right v -> return $ responseBody v
        Left  e -> do
            liftIO $ print e
            logLocM ErrorS ((logStr . show) e)
            liftIO $ threadDelay 5000000
            request proxy url options body
