module Types.Account
    ( Accounts(..)
    , Account(..)
    )
where

import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                )

import           NRQL.Aeson                     ( foldParsers
                                                , responseResults
                                                , accountFromResults
                                                )

newtype Accounts = Accounts {
        accsList :: [Account]
    } deriving(Show, Eq)

instance FromJSON Accounts where
    parseJSON o = do
        r           <- responseResults o
        rawAccounts <- foldParsers (accountFromResults r <$> ["members"])
        return $ Accounts (Account <$> rawAccounts)

newtype Account = Account {
        accNumber :: Integer
    } deriving(Show, Eq, Ord)

instance ToJSON Account where
    toJSON (Account a) = toJSON a

instance FromJSON Account where
    parseJSON o = do
        r <- responseResults o
        Account <$> foldParsers (accountFromResults r <$> ["min", "max"])
