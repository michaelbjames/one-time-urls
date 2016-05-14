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
    let filePathCache = runRedis conn
    quickHttpServe (site filePathCache)

site :: (Redis RedisResponse -> IO RedisResponse) -> Snap ()
site cache =
    ifTop (writeBS "hello world") <|>
    route [ ("foo", writeBS "bar")
          , ("file/:filePath", (fileHandler cache))
          -- , ("insert/:filePath/:hash", (insertHandler cache))
          , ("echo/:echoparam", echoHandler)
          ] <|>
    dir "static" (F.serveDirectory ".")

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param

error404 :: Snap ()
error404 = do
  modifyResponse $ setResponseStatus 404 "Not Found"
  writeBS "404 Not Found"
  r <- getResponse
  finishWith r

fileHandler :: (Redis RedisResponse -> IO RedisResponse) -> Snap ()
fileHandler cache = do
    maybeFilePath <- getParam "filePath"
    case maybeFilePath of
        Nothing ->  error404
        Just filePath -> do
            response <- (liftIO (cache (get filePath)))
            case response of
                Left err -> do
                    liftIO $ putStrLn $ show (err :: Reply)
                    error404
                Right Nothing -> do
                    liftIO $ putStrLn $ "failed to find file for: " ++ (show filePath)
                    error404
                Right (Just value) ->
                    F.serveFile $ unpack $ (value :: ByteString)


