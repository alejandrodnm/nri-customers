{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

module Config
    ( AppConfig(..)
    , AppM(..)
    , DaemonT(..)
    , Environment(..)
    , makeDBPool
    , Daemon(..)
    , getConnectionStr
    , Repo
    , RepoF(..)
    )
where

import           Control.Concurrent             ( threadDelay )
import           Control.Exception              ( throwIO )
import           Control.Monad.Catch            ( MonadThrow
                                                , MonadCatch
                                                , throwM
                                                )

import           Control.Monad.Except           ( MonadError )
import           Control.Monad.Free             ( Free(Free, Pure) )
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT
                                                , asks
                                                , local
                                                )
import           Data.ByteString                ( ByteString )
import           Database.Persist.Postgresql    ( ConnectionPool
                                                , ConnectionString
                                                , createPostgresqlPool
                                                )
import           Katip                          ( Katip(..)
                                                , KatipContext(..)
                                                , LogContexts
                                                , LogEnv
                                                , Namespace
                                                , runKatipT
                                                )
import           Katip.Instances.MonadLogger
import           Network.HTTP.Req               ( MonadHttp(..)
                                                , Url
                                                , Option
                                                , Scheme(Http)
                                                )
import           Network.Wai.Handler.Warp       ( Port )
import           Servant                        ( ServantErr )
import           Servant.Server                 ( Handler )

import           Logger                         ( LogEnv )
import           Network.HTTP.Req.Client        ( ReqClient )
import           Types.Host                     ( Host )

-- | The runtime environment
data Environment
  = Development
  | Test
  | Production
  deriving (Eq, Show, Read)

-- | This type represents the effects we want to have for our web application.
-- We wrap the standard Servant monad with 'ReaderT Config', which gives us
-- access to the application configuration using the 'MonadReader'
-- interface's 'ask' function.
--
-- By encapsulating the effects in our newtype, we can add layers to the
-- monad stack without having to modify code that uses the current layout.
newtype AppM a = AppM
    { runApp :: ReaderT AppConfig Handler a
    } deriving
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader AppConfig
        , MonadError ServantErr
        )

instance Katip AppM  where
    getLogEnv   = asks cfgLogEnv
    localLogEnv = error "not implemented"

instance KatipContext AppM  where
    getKatipContext   = asks cfgLogContext
    getKatipNamespace = asks cfgLogNamespace
    localKatipContext f (AppM a) = AppM (local g a)
        where g s = s { cfgLogContext = f (cfgLogContext s) }
    localKatipNamespace f (AppM a) = AppM (local g a)
        where g s = s { cfgLogNamespace = f (cfgLogNamespace s) }

data AppConfig = AppConfig
    { cfgLogEnv       :: LogEnv
    , cfgLogContext   :: LogContexts
    , cfgLogNamespace :: Namespace
    , cfgPort         :: Port
    , cfgEnv          :: Environment
    , cfgDBPool       :: ConnectionPool
    , cfgNREndpoint   :: (Url Http, Option Http)
    , cfgRepoInterpreter :: forall r. Repo r -> AppM r
    }

-- | This type represents the effect for the daemon application
newtype DaemonT m a = DaemonT
    { runDaemonT :: ReaderT AppConfig m a
    } deriving
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader AppConfig
        , MonadThrow
        , MonadCatch
        )

class (MonadIO m, Monad m, MonadCatch m, MonadReader AppConfig m , ReqClient m) => Daemon m where
    wait :: m ()

instance (MonadIO m, MonadThrow m, MonadCatch m) => Daemon (DaemonT m) where
    wait = liftIO $ threadDelay 1000000

instance MonadIO m => Katip (DaemonT m)  where
    getLogEnv   = asks cfgLogEnv
    localLogEnv = error "not implemented"

instance MonadIO m => KatipContext (DaemonT m)  where
    getKatipContext   = asks cfgLogContext
    getKatipNamespace = asks cfgLogNamespace
    localKatipContext f (DaemonT a) = DaemonT (local g a)
        where g s = s { cfgLogContext = f (cfgLogContext s) }
    localKatipNamespace f (DaemonT a) = DaemonT (local g a)
        where g s = s { cfgLogNamespace = f (cfgLogNamespace s) }

instance (MonadIO m, MonadThrow m) => MonadHttp (DaemonT m) where
    handleHttpException = throwM

instance (MonadIO m, MonadThrow m, MonadCatch m) => ReqClient (DaemonT m)

-- | Determine the number of connections in our @ConnectionPool@ based on the
-- operating environment.
getPoolSize :: Environment -> Int
getPoolSize Production = 8
getPoolSize _          = 1

getConnectionStr :: Environment -> IO ConnectionString
getConnectionStr Development =
    pure
        "host=localhost port=5432 user=postgres password=postgres dbname=nricustomers"
getConnectionStr Test =
    pure
        "host=localhost port=5432 user=postgres password=postgres dbname=nricustomerstest"

makeDBPool :: Environment -> LogEnv -> IO ConnectionPool
makeDBPool env logEnv = do
    connStr <- getConnectionStr env
    runKatipT logEnv $ createPostgresqlPool connStr (getPoolSize env)

data RepoF next
    = GetHosts ([Host] -> next)
    | Done next
    deriving Functor

type Repo = Free RepoF
