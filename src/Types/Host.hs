module Types.Host
    ( Hosts(..)
    , Host(..)
    , HostsCount(..)
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
import           Data.Traversable               ( sequenceA )
import           Data.Monoid                    ( Sum(..)
                                                , getSum
                                                )
import qualified Data.Vector                   as V
import           Text.Read                      ( readMaybe )

import           Models.Host                    ( Host(..) )
import           NRQL.Aeson                     ( foldParsers
                                                , responseResults
                                                , accountFromResults
                                                )

newtype Hosts = Hosts {
        hList :: [Host]
    } deriving(Show, Eq)

instance FromJSON Hosts where
    parseJSON (Object o) = do
        f <- o .: "facets"
        case f of
            Array v ->
                Hosts <$> sequenceA
                    ((parseJSON :: Value -> Parser Host) <$> V.toList v)

            _ -> fail "array of results expected"

instance FromJSON Host where
    parseJSON (Object o) = do
        r <- o .: "results"
        case r of
            Array v ->
                (Host <$> (o .: "name"))
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
                    <*> parserNothing
            _ -> fail "array of results expected"
      where
        toInt :: Parser (Maybe String) -> Parser (Maybe Int)
        toInt = fmap
            ( ((fmap getSum . fold) . (fmap . fmap) Sum)
            . fmap (readMaybe :: String -> Maybe Int)
            )
        parserNothing :: Parser (Maybe Int)
        parserNothing = return Nothing


getLatestFromResult :: FromJSON a => V.Vector Value -> Int -> Parser (Maybe a)
getLatestFromResult v i = case v V.!? i of
    Just val -> case val of
        Object o -> o .:? "latest"
        _        -> fail "expected object"
    Nothing -> fail "results index out of bound"

newtype HostsCount = HostsCount {
        hCount :: Integer
    } deriving(Show, Eq)

instance FromJSON HostsCount where
    parseJSON o = do
        r <- responseResults o
        HostsCount <$> foldParsers (accountFromResults r <$> ["uniqueCount"])
