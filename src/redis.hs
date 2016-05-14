{-# LANGUAGE OverloadedStrings #-}
module Redis
    ( connection
    , get
    )
where

import Database.Redis
import Data.ByteString

connectionInfo :: ConnectInfo
connectionInfo = defaultConnectInfo

connection :: IO Connection
connection = connect connectionInfo
