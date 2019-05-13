module Types.Account where

import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                )
import           Data.Proxy

import           NRQL.Aeson                     ( foldParsers
                                                , responseResults
                                                , fromResults
                                                )

newtype Accounts = Accounts {
        accsList :: [Account]
    } deriving(Show, Eq)

instance FromJSON Accounts where
    parseJSON o = do
        r           <- responseResults o
        rawAccounts <- foldParsers (fromResults r <$> ["members"])
        return $ Accounts (Account <$> rawAccounts)

accountsP :: Proxy Accounts
accountsP = Proxy

newtype Account = Account {
        accNumber :: Int
    } deriving(Show, Eq, Ord)

instance ToJSON Account where
    toJSON (Account a) = toJSON a

instance FromJSON Account where
    parseJSON o = do
        r <- responseResults o
        Account <$> foldParsers (fromResults r <$> ["min", "max"])

accountP :: Proxy Account
accountP = Proxy
