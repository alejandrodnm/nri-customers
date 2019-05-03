{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE InstanceSigs               #-}

module DaemonSpec where

import           Control.Monad.Catch            ( MonadThrow
                                                , MonadCatch
                                                , throwM
                                                )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT
                                                , runReaderT
                                                , asks
                                                , local
                                                )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , decode
                                                )
import           Data.Maybe                     ( fromJust )
import           Data.Proxy
import           Katip                          ( Katip(..)
                                                , KatipContext(..)
                                                )
import           Network.HTTP.Req               ( MonadHttp(..)
                                                , Url
                                                , Option
                                                , Scheme(Http)
                                                , JsonResponse
                                                , parseUrlHttp
                                                )
import           Test.Hspec

import           Config
import           Daemon
import           Network.HTTP.Req.Client        ( ReqClient(..) )
import           Types.Account                  ( Account(..) )


config :: AppConfig
config = AppConfig { cfgLogEnv       = undefined
                   , cfgLogContext   = undefined
                   , cfgLogNamespace = undefined
                   , cfgPort         = undefined
                   , cfgEnv          = undefined
                   , cfgDBPool       = undefined
                   , cfgNREndpoint = fromJust $ parseUrlHttp "http://localhost"
                   }

-- | This type represents the effect for the daemon application
newtype MockDaemonT m a = MockDaemonT
    { runMockDaemonT :: ReaderT AppConfig m a
    } deriving
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader AppConfig
        , MonadThrow
        , MonadCatch
        )

instance (MonadIO m, MonadThrow m, MonadCatch m) => Daemon (MockDaemonT m) where
    wait = return ()

instance MonadIO m => Katip (MockDaemonT m)  where
    getLogEnv   = asks cfgLogEnv
    localLogEnv = error "not implemented"

instance MonadIO m => KatipContext (MockDaemonT m)  where
    getKatipContext   = asks cfgLogContext
    getKatipNamespace = asks cfgLogNamespace
    localKatipContext f (MockDaemonT a) = MockDaemonT (local g a)
        where g s = s { cfgLogContext = f (cfgLogContext s) }
    localKatipNamespace f (MockDaemonT a) = MockDaemonT (local g a)
        where g s = s { cfgLogNamespace = f (cfgLogNamespace s) }

instance (MonadIO m, MonadThrow m) => MonadHttp (MockDaemonT m) where
    handleHttpException = throwM

instance (MonadIO m, MonadThrow m, MonadCatch m) => ReqClient (MockDaemonT m) where
    reqRequest Proxy url options body =
        case decode "{\"results\":[{\"min\": 3.0}]}" of
            Just response -> return response
            Nothing       -> error "failed"

spec :: Spec
spec = context "Run daemon" $ it "executes correctly" $ do
    r <- runReaderT (runMockDaemonT pipeline) config
    r `shouldBe` ()
