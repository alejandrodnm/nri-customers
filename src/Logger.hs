{-# LANGUAGE OverloadedStrings #-}
module Logger
    ( LogEnv
    , Severity(..)
    , KatipContext
    , logCleanup
    , defaultLogEnv
    , waiLogMiddleware
    , runLoggerWithMsg
    , katipAddNamespace
    , katipAddContext
    , logLocM
    , logStr
    )
where

import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Katip                          ( ColorStrategy(ColorIfTerminal)
                                                , Environment(Environment)
                                                , KatipContext
                                                , LogContexts
                                                , LogEnv
                                                , LogStr
                                                , Namespace
                                                , Severity(..)
                                                , Verbosity(V2)
                                                , closeScribes
                                                , defaultScribeSettings
                                                , initLogEnv
                                                , katipAddContext
                                                , katipAddNamespace
                                                , logLocM
                                                , logMsg
                                                , logStr
                                                , mkHandleScribe
                                                , registerScribe
                                                , runKatipContextT
                                                , runKatipT
                                                )
import           Network.Wai                    ( Middleware
                                                , Request(requestMethod)
                                                )
import           System.IO                      ( stdout )

data LogState = MyState {
    namespace :: Namespace
  , context   :: LogContexts
  , logEnv    :: LogEnv
  }

defaultLogEnv :: Show a => a -> IO LogEnv
defaultLogEnv env = do
    handleScribe <- mkHandleScribe ColorIfTerminal stdout DebugS V2
    env          <- initLogEnv "nri-customers" (toEnv env)
    registerScribe "stdout" handleScribe defaultScribeSettings env

toEnv :: Show a => a -> Environment
toEnv e = Environment (pack . show $ e)

logCleanup :: LogEnv -> IO LogEnv
logCleanup = closeScribes

-- | Web request logger
waiLogMiddleware :: LogEnv -> Middleware
waiLogMiddleware env app req respond = app
    req
    (\res -> runKatipT env $ do
        logMsg "web" InfoS (logStr . show . requestMethod $ req)
        liftIO $ respond res
    )

-- | Runs a katip log effect with a single message
runLoggerWithMsg :: MonadIO m => LogEnv -> Namespace -> LogStr -> m ()
runLoggerWithMsg logEnv context msg =
    runKatipContextT logEnv () context $ logLocM InfoS msg
