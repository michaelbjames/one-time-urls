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
    (ifTop (writeBS "hello world") <|>
        route [ ("file/:fileHash", (fileHandler conn))
              , ("insert/:filePath/:hash", (insertHandler conn))
              ] <|>
        dir "static" (F.serveDirectory "."))

error404 :: Snap ()
error404 = do
  modifyResponse $ setResponseStatus 404 "Not Found"
  writeBS "404 Not Found"
  r <- getResponse
  finishWith r

fileHandler :: Connection -> Snap ()
fileHandler conn = do
    let cache = runRedis conn
    maybeFilePath <- getParam "fileHash"
    case maybeFilePath of
        Nothing ->  error404
        Just fileHash -> do
            getResponse <- (liftIO (cache (get fileHash)))
            delResponse <- liftIO $ cache $ del [fileHash]
            case getResponse of
                Left err -> do
                    liftIO $ putStrLn $ show (err :: Reply)
                    error404
                Right (Just value)->
                    F.serveFile $ unpack $ (value :: ByteString)
                _ -> do
                    liftIO $ putStrLn $ "failed to find file for: " ++ (show fileHash)
                    error404

insertHandler :: Connection -> Snap()
insertHandler conn = do
    let cache = runRedis conn
    maybeValue <- getParam "filePath"
    maybeKey <- getParam "hash"
    case (maybeKey, maybeValue) of
        (Just key, Just value) -> do
            redisResponse <- liftIO (cache (set key value))
            case redisResponse of
                Right Ok -> writeBS "OK"
                _ -> writeBS "NOT OK"
        _ -> error404

