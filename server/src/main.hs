{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main (main) where

import Snap
import qualified Snap.Util.FileServe as F
import Control.Applicative ((<|>), (<$>))
import Control.Monad.Trans (liftIO)
import Data.ByteString.Char8 (ByteString, unpack)

import Redis (connection)
import Database.Redis

type RedisResponse = Either Reply (Maybe ByteString)

main :: IO ()
main = do
    conn <- connection
    quickHttpServe (site conn)

site :: Connection  -> Snap ()
site conn =
    ifTop (route
        [ ("file/:fileHash", method GET (fileHandler conn))
        , ("insert/:filePath/:hash", method POST (insertHandler conn))
        ])

error404 :: Snap ()
error404 = do
  modifyResponse $ setResponseStatus 404 "Not Found"
  writeBS "404 Not Found"

fileHandler :: Connection -> Snap ()
fileHandler conn = do
     let cache = runRedis conn
     let redisOp = liftIO . cache
     maybeFilePath <- getParam "fileHash"
     case maybeFilePath of
         Nothing ->  error404
         Just fileHash -> do
             liftIO $ print ("serving " ++ show fileHash)
             getResponse <- redisOp $ get fileHash
             delResponse <- redisOp $ del [fileHash]
             case getResponse of
                 Right (Just value)->
                     F.serveFile $ unpack (value :: ByteString)
                 Left err -> do
                     liftIO $ print (err :: Reply)
                     error404
                 _ -> do
                     liftIO $ print ("failed to find file for: " ++ show fileHash)
                     error404

insertHandler :: Connection -> Snap ()
insertHandler conn = do
    let cache = runRedis conn
    maybeValue <- getParam "filePath"
    maybeKey <- getParam "hash"
    liftIO $ print maybeValue
    case (maybeKey, maybeValue) of
        (Just key, Just value) -> do
            redisResponse <- liftIO (cache (set key value))
            case redisResponse of
                Right Ok -> do
                    liftIO (putStrLn $ "Wrote: " ++ show value ++ " to " ++ show key)
                    writeBS "OK"
                _ -> do
                    liftIO (putStrLn $ "FAILED to write: " ++ show value ++ " to " ++ show key)
                    writeBS "NOT OK"
        _ -> error404

