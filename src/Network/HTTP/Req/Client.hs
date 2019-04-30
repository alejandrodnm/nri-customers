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
import           Network.HTTP.Req               ( JsonResponse
                                                , MonadHttp
                                                , POST(POST)
                                                , ReqBodyJson(ReqBodyJson)
                                                , Url
                                                , Option
                                                , jsonResponse
                                                , req
                                                )

import           Logger                         ( KatipContext
                                                , Severity(..)
                                                , logLocM
                                                , logStr
                                                )

class (KatipContext m, MonadHttp m, MonadCatch m, MonadIO m ) => (ReqClient m) where
    reqRequest :: (ToJSON a, FromJSON b) => Url scheme -> Option scheme -> a -> m (JsonResponse b)
    reqRequest = request

-- Executes a request with the given `DiracRequest`, in case of and
-- error the request will be retried.
request
    :: forall a m b scheme
     . (KatipContext m, MonadHttp m, MonadCatch m, FromJSON b, ToJSON a)
    => Url scheme
    -> Option scheme
    -> a
    -> m (JsonResponse b)
request url options body = do
    logLocM DebugS ((logStr . show . encode) body)
    r <-
        try $ req POST url (ReqBodyJson body) jsonResponse options :: m
            (Either SomeException (JsonResponse b))
    case r of
        Right v -> return v
        Left  e -> do
            liftIO $ print e
            logLocM ErrorS ((logStr . show) e)
            liftIO $ threadDelay 5000000
            request url options body
