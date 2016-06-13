{-# LANGUAGE OverloadedStrings #-}
module Client (main) where

import Network.HTTP (simpleHTTP, postRequest)
import Crypto.Hash (hashlazy, MD5, Digest)
import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Data.List (elem)
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)


data Options = Options
    { host :: String
    , port :: Integer
    , maybeHash :: Maybe String
    , maybeFilename :: Maybe String
    } deriving (Show)

defaultOptions = Options
    { host="127.0.0.1"
    , port=8000
    , maybeHash=Nothing
    , maybeFilename=Nothing
    }

md5 :: String -> String
md5 = show . (hashlazy :: ByteString -> Digest MD5) . pack

main = do
    args <- getArgs
    options <- parseArgs args
    filename <- getValidFilename options
    hashString <- case maybeHash options of
        Nothing -> readFile filename >>= return . md5
        Just hash -> return hash
    sendToServer filename hashString (host options) (port options)

sendToServer :: String -> String -> String -> Integer -> IO ()
sendToServer filename hashString hostName portNum = do
    let hostPort = "http://" ++ hostName ++ ":" ++ (show portNum)
    let urlFilePath = "/insert/" ++ filename ++ "/" ++ hashString
    let url = hostPort ++ urlFilePath
    putStrLn url
    let request = postRequest url
    result <- simpleHTTP request
    putStrLn (show result)
    return ()

getValidFilename :: Options -> IO String
getValidFilename options =
    case (maybeFilename options) of
        Nothing -> do
            putStrLn usage
            exitFailure
        Just filename -> return filename

parseArgs :: [String] -> IO Options
parseArgs ("--help":as) = putStrLn usage >> exitSuccess
parseArgs ("--port":n:as) = do
    putStrLn "port given"
    options <- parseArgs as
    return (options {port = read n})
parseArgs ("--name":n:as) = do
    putStrLn "name given"
    options <- parseArgs as
    return (options {maybeHash = Just n})
parseArgs ("--host":h:as) = do
    putStrLn "host given"
    options <- parseArgs as
    return (options {host = h})
parseArgs (filename:as) = do
    options <- parseArgs as
    return (options {maybeFilename = Just filename})
parseArgs [] = return defaultOptions


usage :: String
usage = unlines
    [ "Client one time URL library."
    , "otu <filename> [--port n] [--name nameInsteadOfHash]"
    ]
