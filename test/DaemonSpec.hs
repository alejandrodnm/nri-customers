{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE InstanceSigs               #-}

module DaemonSpec where

import           Control.Monad.Catch            ( MonadThrow
                                                , MonadCatch
                                                , throwM
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT
                                                , runReaderT
                                                , asks
                                                , local
                                                )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , decode
                                                , encode
                                                )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import           Data.Maybe                     ( fromJust )
import           Data.Proxy
import           Data.Word                      ( Word8 )
import           Database.Persist (entityVal)
import           Database.Persist.Sql           ( (==.) )
import           Database.Persist.Postgresql    ( selectFirst
                                                , toSqlKey
                                                )
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
import           Logger
import           Models.Host
import           Network.HTTP.Req.Client        ( ReqClient(..) )
import           Persist                        ( migrate )
import           Types.Account                  ( Account(..) )
import           Test.Persist


mockConfig :: IO AppConfig
mockConfig = do
    let env      = Test
        endpoint = fromJust $ parseUrlHttp "http://localhost"
    logEnv <- defaultLogEnv env
    dbPool <- makeDBPool env logEnv
    migrate dbPool
    return AppConfig { cfgEnv          = env
                     , cfgLogEnv       = logEnv
                     , cfgLogContext   = mempty
                     , cfgLogNamespace = mempty
                     , cfgPort         = undefined
                     , cfgDBPool       = dbPool
                     , cfgNREndpoint   = endpoint
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
    reqRequest Proxy url options body = do
        let body' = BL.toStrict $ encode body
        liftIO $ print body'
        case decode $ mockedRequestPayload body' of
            Just response -> return response
            Nothing       -> error "failed"

mockedRequestPayload :: B.ByteString -> BL.ByteString
mockedRequestPayload body
    | "\"query\":\"select min(account) from ArchiveFile where storedEventType = 'SystemSample' since 1 week ago limit 1000\""
        `B.isInfixOf` body
    = "{\"results\":[{\"min\": 3.0}]}"
    | "\"query\":\"select max(account) from ArchiveFile where storedEventType = 'SystemSample' since 1 week ago limit 1000\""
        `B.isInfixOf` body
    = "{\"results\":[{\"min\": 42.0}]}"
    | "\"query\":\"select uniques(account) from ArchiveFile where storedEventType = 'SystemSample' AND account >= 3 AND account < 1003 since 1 week ago limit 1000\""
        `B.isInfixOf` body
    = "{\"results\":[{\"members\": [33.0, 34.0]}]}"
    | "\"query\":\"select uniqueCount(entityId) from SystemSample where true since 1 week ago limit 1000\""
        `B.isInfixOf` body
    = "{\"results\":[{\"uniqueCount\": 2}]}"
    | "\"query\":\"select latest(linuxDistribution), latest(agentVersion), latest(kernelVersion), latest(instanceType), latest(operatingSystem), latest(windowsVersion), latest(windowsPlatform), latest(windowsFamily), latest(coreCount), latest(processorCount), latest(systemMemoryBytes) from SystemSample where true facet entityId since 1 week ago limit 1000\""
        `B.isInfixOf` body
    = "{\"facets\":[{\"name\":\"974205676942181148\",\"results\":[{\"latest\":\"CentOSLinux7(Core)\"},{\"latest\":\"1.3.20\"},{\"latest\":\"3.10.0-514.21.1.el7.x86_64\"},{\"latest\":\"unknown\"},{\"latest\":\"linux\"},{\"latest\":null},{\"latest\":null},{\"latest\":null},{\"latest\":\"14\"},{\"latest\":\"56\"},{\"latest\":\"269033377792\"}]},{\"name\":\"8923018336323236291\",\"results\":[{\"latest\":\"ContainerLinuxbyCoreOS1911.5.0(Rhyolite)\"},{\"latest\":\"1.0.1019\"},{\"latest\":\"4.14.84-coreos\"},{\"latest\":\"SupermicroSYS-6019U-TN4R4T\"},{\"latest\":\"linux\"},{\"latest\":null},{\"latest\":null},{\"latest\":null},{\"latest\":\"18\"},{\"latest\":\"72\"},{\"latest\":\"404430925824\"}]}]}"
    | otherwise
    = undefined

spec :: Spec
spec = before_ flushDB $ context "Run daemon" $ it "executes correctly" $ do
    r <- runReaderT (runMockDaemonT pipeline) =<< mockConfig
    mHost <- runDBQuery $ selectFirst [HostEntityId ==. "974205676942181148"] []
    -- TODO better assertions
    (hostEntityId $ entityVal $ fromJust mHost) `shouldBe` "974205676942181148"
    r `shouldBe` ()
