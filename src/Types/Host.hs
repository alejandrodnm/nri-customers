module Types.Host
    ( Hosts(..)
    , Host(..)
    , PartialHost(..)
    , HostsCount(..)
    , hostFromPartial
    , hostsCountP
    , hostsP
    )
where

import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                , Value
                                                , Value(Array, Object)
                                                , (.:)
                                                , (.:?)
                                                )
import           Data.Aeson.Types               ( Parser )
import           Data.Foldable                  ( fold )
import           Data.Monoid                    ( Sum(..)
                                                , getSum
                                                )
import           Data.Proxy
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Data.Traversable               ( sequenceA )
import qualified Data.Vector                   as V
import           Text.Read                      ( readMaybe )

import           Models.Host                    ( Host(..) )
import           Types.Account                  ( Account(..) )
import           NRQL.Aeson                     ( foldParsers
                                                , responseResults
                                                , accountFromResults
                                                )

newtype Hosts = Hosts {
        hList :: [PartialHost]
    } deriving(Show, Eq)

hostsP :: Proxy Hosts
hostsP = Proxy

data PartialHost = PartialHost
    { entityId :: Text
    , linuxDistribution :: Maybe String
    , agentVersion      ::  Maybe String
    , kernelVersion     ::  Maybe String
    , instanceType      ::  Maybe String
    , operatingSystem   ::  Maybe String
    , windowsVersion    ::  Maybe String
    , windowsPlatform   ::  Maybe String
    , windowsFamily     ::  Maybe String
    , coreCount         :: Maybe Int
    , processorCount    :: Maybe Int
    , systemMemoryBytes :: Maybe Int
    } deriving(Show, Eq)


instance FromJSON Hosts where
    parseJSON (Object o) = do
        f <- o .: "facets"
        case f of
            Array v -> Hosts <$> sequenceA
                ((parseJSON :: Value -> Parser PartialHost) <$> V.toList v)

            _ -> fail "array of results expected"

instance FromJSON PartialHost where
    parseJSON (Object o) = do
        r <- o .: "results"
        case r of
            Array v ->
                (PartialHost <$> (o .: "name"))
                    <*> getLatestFromResult v 0
                    <*> getLatestFromResult v 1
                    <*> getLatestFromResult v 2
                    <*> getLatestFromResult v 3
                    <*> getLatestFromResult v 4
                    <*> getLatestFromResult v 5
                    <*> getLatestFromResult v 6
                    <*> getLatestFromResult v 7
                    <*> toInt (getLatestFromResult v 8)
                    <*> toInt (getLatestFromResult v 9)
                    <*> toInt (getLatestFromResult v 10)
            _ -> fail "array of results expected"
      where
        toInt :: Parser (Maybe String) -> Parser (Maybe Int)
        toInt = fmap
            ( ((fmap getSum . fold) . (fmap . fmap) Sum)
            . fmap (readMaybe :: String -> Maybe Int)
            )

getLatestFromResult :: FromJSON a => V.Vector Value -> Int -> Parser (Maybe a)
getLatestFromResult v i = case v V.!? i of
    Just val -> case val of
        Object o -> o .:? "latest"
        _        -> fail "expected object"
    Nothing -> fail "results index out of bound"

newtype HostsCount = HostsCount {
        hCount :: Int
    } deriving(Show, Eq)

instance FromJSON HostsCount where
    parseJSON o = do
        r <- responseResults o
        HostsCount <$> foldParsers (accountFromResults r <$> ["uniqueCount"])

hostsCountP :: Proxy HostsCount
hostsCountP = Proxy

hostFromPartial :: Account -> UTCTime -> PartialHost -> Host
hostFromPartial a time ph = Host { hostEntityId          = entityId ph
                                 , hostAccount           = accNumber a
                                 , hostLinuxDistribution = linuxDistribution ph
                                 , hostAgentVersion      = agentVersion ph
                                 , hostKernelVersion     = kernelVersion ph
                                 , hostInstanceType      = instanceType ph
                                 , hostOperatingSystem   = operatingSystem ph
                                 , hostWindowsVersion    = windowsVersion ph
                                 , hostWindowsPlatform   = windowsPlatform ph
                                 , hostWindowsFamily     = windowsFamily ph
                                 , hostCoreCount         = coreCount ph
                                 , hostProcessorCount    = processorCount ph
                                 , hostSystemMemoryBytes = systemMemoryBytes ph
                                 , hostCreated           = time
                                 }
