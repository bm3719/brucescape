{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Network.URI (parseURI, uriScheme, uriAuthority, uriRegName, pathSegments)
import System.Environment

-- testUrl :: String
-- testUrl = "https://www.example.com:8080/path/to/page?query=value#fragment"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "No URL specified"
    url:_ -> do
      putStrLn $ "Navigating to: " ++ url
      let parsedUrl = parseURI url
          scheme = (fromMaybe "Invalid URL" $ uriScheme <$> parsedUrl)
          host = maybe "Invalid URL" uriRegName (uriAuthority =<< parseURI url)
          path = (fromMaybe "Invalid URL" $
                  (('/' :) . intercalate "/" . pathSegments) <$> parsedUrl)
      putStrLn $ "URI Scheme: " ++ scheme
      putStrLn $ "Host: " ++ host
      putStrLn $ "Path: " ++ path
