{-# LANGUAGE OverloadedStrings #-}

module Daemon
    ( daemonLoop
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

import           Config                         ( DaemonT )
import           Logger                         ( Severity(..)
                                                , katipAddNamespace
                                                , logLocM
                                                , logStr
                                                )
import           NRQL.Client                    ( ArchiveFunction(..)
                                                , getAccount
                                                , getHostsCount
                                                , hostsCountQueryFilteredByEntity
                                                , hostsCountQuery
                                                , hostsQueryFilteredByEntity
                                                , hostsQuery
                                                , getAccounts
                                                , getHosts
                                                , nrqlLimit
                                                )
import           Persist                        ( runDB )
import           Types.Account                  ( Account(..)
                                                , Accounts(..)
                                                )
import           Types.Host                     ( Hosts(hList)
                                                , Host
                                                , hostFromPartial
                                                )

daemonLoop :: DaemonT IO ()
daemonLoop = katipAddNamespace "daemon" $ forever $ do
    pipeline
    liftIO $ threadDelay week

wait :: DaemonT IO ()
wait = liftIO $ threadDelay week

week = 7 * 24 * 3600 * 1000000

pipeline :: DaemonT IO ()
pipeline = do

    minAccount <- getAccount Min
    logLocM DebugS (logStr $ "min account retrieved: " ++ show minAccount)
    wait

    maxAccount <- getAccount Max
    logLocM DebugS (logStr $ "max account retrieved: " ++ show maxAccount)
    wait

    time <- liftIO getCurrentTime
    processAccounts maxAccount minAccount time
    return ()

nextTop :: Account -> Account
nextTop account = Account (accNumber account + nrqlLimit)

processAccounts :: Account -> Account -> UTCTime -> DaemonT IO ()
processAccounts max bottom time = do
    let top = nextTop bottom
    logLocM
        DebugS
        (logStr $ "starting pipeline from " ++ show bottom ++ " to " ++ show top
        )
    accounts <- getAccounts bottom top
    wait
    processAccountHosts time accounts
    when (top < max) $ processAccounts max top time

processAccountHosts :: UTCTime -> Accounts -> DaemonT IO ()
processAccountHosts _    (Accounts []      ) = return ()
processAccountHosts time (Accounts (a : as)) = do
    hostsCount <- getHostsCount a hostsCountQuery
    wait
    case () of
        _
            | hostsCount > 0 && hostsCount <= nrqlLimit -> do
                partialHosts <- getHosts a hostsQuery
                wait
                persistHosts a time partialHosts
                return hostsCount
            | hostsCount > nrqlLimit -> processAccountHostsPagination
                a
                time
                hostsCount
            | otherwise -> return hostsCount
    processAccountHosts time (Accounts as)

persistHosts :: Account -> UTCTime -> Hosts -> DaemonT IO [Key Host]
persistHosts a time partialHosts = do
    let hosts = hostFromPartial a time <$> hList partialHosts
    runDB (insertMany hosts)

processAccountHostsPagination :: Account -> UTCTime -> Int -> DaemonT IO Int
processAccountHostsPagination a time total =
    processAccountHostsPagination' a time total 0 "" 1

processAccountHostsPagination'
    :: Account -> UTCTime -> Int -> Int -> String -> Int -> DaemonT IO Int
processAccountHostsPagination' a time total totalAcc partialEntity i
    | totalAcc >= total = return totalAcc
    | i >= 10 = return totalAcc
    | otherwise = do
        let newPartialEntity = partialEntity ++ show i
        hostsCount <- getHostsCount
            a
            (hostsCountQueryFilteredByEntity newPartialEntity)
        wait
        retrievedEntities <- case () of
            _
                | hostsCount > 0 && hostsCount <= nrqlLimit -> do
                    partialHosts <- getHosts
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
