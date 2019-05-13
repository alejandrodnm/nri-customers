{-# LANGUAGE FlexibleContexts #-}

module Persist
    ( migrate
    , runDB
    )
where

import           Config                         ( AppConfig
                                                , cfgDBPool
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Reader           ( MonadReader
                                                , asks
                                                )
import           Database.Persist.Postgresql    ( ConnectionPool
                                                , SqlPersistT
                                                , get
                                                , runMigration
                                                , runSqlPool
                                                )
import qualified Models.Host                   as H

migrate :: ConnectionPool -> IO ()
migrate = runSqlPool $ runMigration H.migrateAll

runDB :: (MonadReader AppConfig m, MonadIO m) => SqlPersistT IO a -> m a
runDB query = do
    pool <- asks cfgDBPool
    liftIO $ runSqlPool query pool
