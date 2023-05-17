{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import qualified Data.ByteString.Char8     as BS
import           Data.List                 (intercalate, stripPrefix)
import           Data.Maybe                (fromMaybe)
import           Network.Socket
import           Network.Socket.ByteString (sendAll, recv)
import           Network.URI
import           System.Environment

-- testUrl :: String
-- testUrl = "http://www.example.com:8080/path/to/page?query=value#fragment"

createSocket :: String -> String -> IO (Socket, AddrInfo)
createSocket scheme host = do
  let hints = defaultHints { addrFlags = [AI_ADDRCONFIG], addrSocketType = Stream }
  addr:_ <- getAddrInfo (Just hints) (Just host) (Just scheme)
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  return (sock, addr)

stripSuffix :: String -> String -> Maybe String
stripSuffix suffix str = reverse <$> stripPrefix (reverse suffix) (reverse str)

dropLastColon :: String -> String
dropLastColon = fromMaybe <*> stripSuffix ":"

main :: IO ()
main = do
  args <- getArgs
  (\case
    [] -> putStrLn "No URL specified"
    url:_ -> do
      putStrLn $ "Navigating to: " ++ url
      let parsedUrl = parseURI url
          scheme = (fromMaybe "Invalid URL" $ (dropLastColon . uriScheme) <$> parsedUrl)
          host = maybe "Invalid URL" uriRegName (uriAuthority =<< parseURI url)
          path = (fromMaybe "Invalid URL" $
                  (('/' :) . intercalate "/" . pathSegments) <$> parsedUrl)
      putStrLn $ "URI Scheme: " ++ scheme
      putStrLn $ "Host: " ++ host
      putStrLn $ "Path: " ++ path

      -- Create network connection to host.
      (sock, addr) <- createSocket scheme host
      connect sock (addrAddress addr)
      let request = "GET " ++ path ++ " HTTP/1.1\r\nHost: " ++ host ++ "\r\n\r\n"
      sendAll sock (BS.pack request)
      -- Note: Maximum size in bytes of buffer to receive statically defined.
      --       Use Network.Socket.ByteString.Lazy if this is an issue.
      response <- recv sock 4096
      putStrLn $ BS.unpack response
      close sock
    ) args
