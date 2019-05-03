module Daemon
    ( daemonLoop
    , pipeline
    )
where

import           Control.Concurrent             ( threadDelay )
import           Control.Monad                  ( forever
                                                , when
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Maybe                     ( fromJust )
import           Data.Time.Clock                ( UTCTime
                                                , getCurrentTime
                                                )
import           Database.Persist.Class         ( insertMany )
import           Database.Persist.Types         ( Key )
import           Network.HTTP.Req               ( responseBody )

import           Config                         ( DaemonT
                                                , Daemon(..)
                                                )
import           Logger                         ( Severity(..)
                                                , katipAddNamespace
                                                , logLocM
                                                , logStr
                                                )
import           NRQL.Client                    ( runQuery
                                                , metaAccount
                                                )
import           NRQL.Query                     ( ArchiveFunction(..)
                                                , hostsCountQueryFilteredByEntity
                                                , hostsCountQuery
                                                , hostsQueryFilteredByEntity
                                                , hostsQuery
                                                , nrqlLimit
                                                , accountQuery
                                                , accountsQuery
                                                )
import           Persist                        ( runDB )
import           Types.Account                  ( Account(..)
                                                , Accounts(..)
                                                , accountP
                                                , accountsP
                                                )
import           Types.Host                     ( Hosts(hList)
                                                , Host
                                                , HostsCount(..)
                                                , hostFromPartial
                                                , hostsCountP
                                                , hostsP
                                                )
import           Data.Proxy

daemonLoop :: DaemonT IO ()
daemonLoop = katipAddNamespace "daemon" $ forever $ do
    pipeline
    liftIO $ threadDelay week

week = 7 * 24 * 3600 * 1000000

pipeline :: Daemon m => m ()
pipeline = do

    minAccount <- runQuery accountP metaAccount (accountQuery Min)
    logLocM DebugS (logStr $ "min account retrieved: " ++ show minAccount)
    wait

    maxAccount <- runQuery accountP metaAccount (accountQuery Max)
    logLocM DebugS (logStr $ "max account retrieved: " ++ show maxAccount)
    wait

    time <- liftIO getCurrentTime
    processAccounts maxAccount minAccount time
    return ()

nextTop :: Account -> Account
nextTop account = Account (accNumber account + nrqlLimit)

processAccounts :: Daemon m => Account -> Account -> UTCTime -> m ()
processAccounts max bottom time = do
    let top = nextTop bottom
    logLocM
        DebugS
        (logStr $ "starting pipeline from " ++ show bottom ++ " to " ++ show top
        )
    accounts <- runQuery accountsP metaAccount (accountsQuery bottom top)
    wait
    processAccountHosts time accounts
    when (top < max) $ processAccounts max top time

processAccountHosts :: Daemon m => UTCTime -> Accounts -> m ()
processAccountHosts _    (Accounts []      ) = return ()
processAccountHosts time (Accounts (a : as)) = do
    hostsCount <- hCount <$> runQuery hostsCountP a hostsCountQuery
    wait
    case () of
        _
            | hostsCount > 0 && hostsCount <= nrqlLimit -> do
                partialHosts <- runQuery hostsP a hostsQuery
                wait
                persistHosts a time partialHosts
                return hostsCount
            | hostsCount > nrqlLimit -> processAccountHostsPagination
                a
                time
                hostsCount
            | otherwise -> return hostsCount
    processAccountHosts time (Accounts as)

persistHosts :: Daemon m => Account -> UTCTime -> Hosts -> m [Key Host]
persistHosts a time partialHosts = do
    let hosts = hostFromPartial a time <$> hList partialHosts
    runDB (insertMany hosts)

processAccountHostsPagination :: Daemon m => Account -> UTCTime -> Int -> m Int
processAccountHostsPagination a time total =
    processAccountHostsPagination' a time total 0 "" 1

processAccountHostsPagination'
    :: Daemon m => Account -> UTCTime -> Int -> Int -> String -> Int -> m Int
processAccountHostsPagination' a time total totalAcc partialEntity i
    | totalAcc >= total = return totalAcc
    | i >= 10 = return totalAcc
    | otherwise = do
        let newPartialEntity = partialEntity ++ show i
        hostsCount <- hCount <$> runQuery
            hostsCountP
            a
            (hostsCountQueryFilteredByEntity newPartialEntity)
        wait
        retrievedEntities <- case () of
            _
                | hostsCount > 0 && hostsCount <= nrqlLimit -> do
                    partialHosts <- runQuery
                        hostsP
                        a
                        (hostsQueryFilteredByEntity newPartialEntity)
                    wait
                    persistHosts a time partialHosts
                    return hostsCount
                | hostsCount > nrqlLimit -> processAccountHostsPagination'
                    a
                    time
                    total
                    totalAcc
                    newPartialEntity
                    1
                | otherwise -> return hostsCount
        processAccountHostsPagination' a
                                       time
                                       total
                                       (totalAcc + retrievedEntities)
                                       partialEntity
                                       (i + 1)
