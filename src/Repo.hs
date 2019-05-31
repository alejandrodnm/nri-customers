{-# LANGUAGE FlexibleContexts #-}

module Repo where

import           Control.Monad.Free             ( Free(Free, Pure)
                                                , liftF
                                                )
import           Control.Monad.Reader           ( MonadReader )
import           Control.Monad.IO.Class         ( MonadIO )
import           Database.Esqueleto
import           Database.Persist.Postgresql    ( SqlPersistT )

import           Config                         ( Repo
                                                , RepoF(..)
                                                , AppConfig
                                                )

import           Persist                        ( runDB )
import           Types.Host                     ( Host )

retrieveHosts :: Repo [Host]
retrieveHosts = liftF (GetHosts id)

hostsQuery :: SqlPersistT IO [Host]
hostsQuery = do
    hosts <- select $ from $ \host -> return host
    return $ entityVal <$> hosts

interpret :: (MonadReader AppConfig m, MonadIO m) => Repo r -> m r
interpret repo = case repo of
    Free (GetHosts g) -> do
        hosts <- runDB hostsQuery
        interpret (g hosts)
    Pure r -> return r
