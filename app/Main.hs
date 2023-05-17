{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Maybe (fromMaybe)
import Network.URI (parseURI, uriScheme, uriAuthority, uriRegName)
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
      putStrLn $ "URI Scheme: " ++ (fromMaybe "Invalid URL" $ uriScheme <$> parsedUrl)
      putStrLn $ "Host: " ++ maybe "Invalid URL" uriRegName (uriAuthority =<< parseURI url)
