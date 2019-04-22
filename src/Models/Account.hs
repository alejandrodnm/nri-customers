{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Account where

import           Data.Aeson       (FromJSON (..), ToJSON (..), Value,
                                   Value (Array, Object), decode, encode, (.:),
                                   (.:?))
import           Data.Aeson.Types (Parser)
import           Data.ByteString  (ByteString)
import           Data.Text        (Text)
import qualified Data.Vector      as V
import           GHC.Generics     (Generic)

newtype Accounts = Accounts {
        accsList :: [Account]
    } deriving(Show, Eq)

instance FromJSON Accounts where
    parseJSON o = do
        r <- responseResults o
        rawAccounts <- foldParsers (accountFromResults r <$> ["members"])
        return $ Accounts (Account  <$> rawAccounts)

newtype Account = Account {
        accNumber :: Integer
    } deriving(Show, Eq, Ord)

newtype EntitiesCount = EntitiesCount {
        ecCount :: Integer
    } deriving(Show)

instance FromJSON EntitiesCount where
    parseJSON o = do
        r <- responseResults o
        EntitiesCount <$> foldParsers (accountFromResults r <$> ["uniqueCount"])

instance ToJSON Account where
    toJSON (Account a)  = toJSON a

instance FromJSON Account where
    parseJSON o = do
        r <- responseResults o
        Account <$> foldParsers (accountFromResults r <$> ["min", "max"])

-- Returns the first valid parser, is no parser is valid, it fails.
foldParsers :: (FromJSON b) => [Parser (Maybe b)] -> Parser b
foldParsers []       = fail "none of the allowed functions attributes was found"
foldParsers (p : ps) = p >>= maybe (foldParsers ps) return

-- Returns a parser up to the `results` attribute of the response
responseResults :: Value -> Parser (V.Vector Value)
responseResults v = case v of
    Object o -> do
        r <- o .: "results"
        case r of
            Array v -> return v
            _       -> fail "array of results expected"
    _ -> fail "root object expected"

accountFromResults :: (FromJSON b) => V.Vector Value -> Text -> Parser (Maybe b)
accountFromResults v f = case length v of
    0 -> fail "results array from query is empty"
    _ -> case V.head v of
        Object m -> m .:? f
        _        -> fail "array of result objects expected"
