module Api.HostSpec where

import qualified Control.Concurrent            as C
import           Control.Exception              ( bracket )
import           Control.Monad.Free             ( Free(Free, Pure) )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import qualified Network.Wai.Handler.Warp      as Warp
import           Network.HTTP.Client     hiding ( Proxy )
import           Servant
import           Servant.Client
import           Servant.Server
import           Data.Time.Clock                ( UTCTime
                                                , getCurrentTime
                                                )

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Matcher

import           Api
import           Api.Host
import           Config
import           Logger
import           Types.Host



import           Data.Time.Calendar
import           Data.Time

withUserApp :: IO () -> IO ()
withUserApp action = do
    cfg <- mockConfig
    bracket
        (liftIO $ C.forkIO $ Warp.runSettings
            (Warp.setHost "localhost" $ Warp.setPort 8888 Warp.defaultSettings)
            (app cfg)
        )
        C.killThread
        (const action)

mockConfig :: IO AppConfig
mockConfig = do
    let env = Test
    logEnv <- defaultLogEnv env
    return AppConfig { cfgEnv             = env
                     , cfgLogEnv          = logEnv
                     , cfgLogContext      = mempty
                     , cfgLogNamespace    = mempty
                     , cfgPort            = undefined
                     , cfgDBPool          = undefined
                     , cfgNREndpoint      = undefined
                     , cfgRepoInterpreter = interpret
                     }

interpret :: (MonadIO m) => Repo r -> m r
interpret repo = case repo of
    Free (GetHosts g) -> do
        let hosts = []
        interpret (g mockHosts)
    Pure r -> return r

mockHosts :: [Host]
mockHosts =
    let utcTime = UTCTime (fromGregorian 2018 10 27) (secondsToDiffTime 0)
    in  [ Host { hostEntityId          = "entityID"
               , hostAccount           = 42
               , hostLinuxDistribution = Just "Fedora"
               , hostAgentVersion      = Just "1.3.27"
               , hostKernelVersion     = Just "2.3"
               , hostInstanceType      = Just "Big one"
               , hostOperatingSystem   = Just "Linux"
               , hostWindowsVersion    = Nothing
               , hostWindowsPlatform   = Nothing
               , hostWindowsFamily     = Nothing
               , hostCoreCount         = Just 8
               , hostProcessorCount    = Just 4
               , hostSystemMemoryBytes = Just 4096
               , hostCreated           = utcTime
               }
        ]

spec :: Spec
spec = around_ withUserApp $ do
    -- create a test client function
    let hostClient = client (Proxy :: Proxy HostAPI)
    -- create a servant-client ClientEnv
    baseUrl <- runIO $ parseBaseUrl "http://localhost:8888"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv = mkClientEnv manager baseUrl

    describe "GET /host" $ it "should retrieve hosts" $ do
        result <- runClientM hostClient clientEnv
        t      <- liftIO getCurrentTime
        result `shouldBe` Right mockHosts
