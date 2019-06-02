{-# LANGUAGE DataKinds                  #-}

module Init
    ( runApp
    )
where

import           Control.Concurrent             ( forkIO )
import           Control.Exception              ( bracket )
import           Control.Monad.Reader           ( runReaderT )
import           Daemon                         ( daemonLoop )
import           Data.ByteString                ( ByteString )
import           Data.ByteString.Char8          ( pack )
import           Data.Maybe                     ( fromMaybe )
import           Network.HTTP.Req               ( parseUrlHttp
                                                , Url
                                                , Option
                                                , Scheme(Http)
                                                )
import           Network.Wai.Handler.Warp       ( Port
                                                , Settings
                                                , defaultSettings
                                                , runSettings
                                                , setHost
                                                , setPort
                                                )
import           Safe                           ( readMay )
import           System.Environment             ( lookupEnv )

import           Api                            ( app )
import           Config                         ( AppConfig(..)
                                                , Environment(..)
                                                , makeDBPool
                                                , DaemonT(runDaemonT)
                                                )
import           Logger                         ( LogEnv
                                                , defaultLogEnv
                                                , logCleanup
                                                , runLoggerWithMsg
                                                , waiLogMiddleware
                                                )
import           Persist                        ( migrate )
import           Repo                           ( interpret )


-- | Returns a default warp settings but with localhost as host
-- this is convenient to not trigger the macOS firewall
warpSettings :: Port -> Settings
warpSettings port = setHost "localhost" $ setPort port defaultSettings

runApp :: IO ()
runApp = bracket getConfig shutdown runApp'
  where
    runApp' :: AppConfig -> IO ()
    runApp' config = do
        let logEnv = cfgLogEnv config
        forkIO $ do
            runLoggerWithMsg logEnv "daemon" "starting daemon"
            runReaderT (runDaemonT daemonLoop) config
        runLoggerWithMsg logEnv "main" "starting web server"
        runSettings (warpSettings $ cfgPort config)
                    (waiLogMiddleware logEnv $ app config)

getConfig :: IO AppConfig
getConfig = do
    env      <- lookupSetting "ENV" Development
    logEnv   <- defaultLogEnv env
    port     <- lookupSetting "PORT" 8000
    endpoint <- lookupStringSetting "NR_ENDPOINT" "http://localhost:8000/dirac"
    dbPool   <- makeDBPool env logEnv
    migrate dbPool
    return AppConfig { cfgEnv             = env
                     , cfgLogEnv          = logEnv
                     , cfgLogContext      = mempty
                     , cfgLogNamespace    = mempty
                     , cfgPort            = port
                     , cfgDBPool          = dbPool
                     , cfgNREndpoint      = parseURL endpoint
                     , cfgRepoInterpreter = interpret
                     }

parseURL :: String -> (Url Http, Option b)
-- FIXME fromMaybe
parseURL url = fromMaybe (handleFailedParse url) (parseUrlHttp $ pack url)
  where
    handleFailedParse url = error $ "Failed to parse url [[" ++ url ++ "]]"

shutdown :: AppConfig -> IO ()
shutdown cfg = do
    logCleanup (cfgLogEnv cfg)
    return ()

-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
lookupStringSetting :: String -> String -> IO String
lookupStringSetting env def = do
    maybeValue <- lookupEnv env
    case maybeValue of
        Nothing  -> return def
        Just str -> return str

-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    maybeValue <- lookupEnv env
    case maybeValue of
        Nothing  -> return def
        Just str -> maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str = error $ mconcat
        ["Failed to read [[", str, "]] for environment variable ", env]
