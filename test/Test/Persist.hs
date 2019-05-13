{-# LANGUAGE ScopedTypeVariables #-}

module Test.Persist where

import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.IO.Unlift        ( MonadUnliftIO )
import           Database.Persist.Sql           ( rawExecute
                                                , runSqlConn
                                                )
import           Database.Persist.Sql.Types.Internal
                                                ( SqlBackend(..) )
import           Database.Persist.Postgresql    ( withPostgresqlConn
                                                , SqlPersistT
                                                )
import           Katip                          ( initLogEnv
                                                , runKatipT
                                                )

import           Config

runDBQuery :: (MonadUnliftIO m, MonadIO m) => SqlPersistT IO a -> m a
runDBQuery query = do
    env     <- liftIO $ initLogEnv "daemon" "test"
    connStr <- liftIO $ getConnectionStr Test
    runKatipT env $ withPostgresqlConn connStr $ \conn ->
        liftIO $ runSqlConn query conn

flushDB :: (MonadUnliftIO m, MonadIO m) => m ()
flushDB = runDBQuery $ rawExecute "TRUNCATE host" []
