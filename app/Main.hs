{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import qualified Data.ByteString.Char8          as BS (pack)
import qualified Data.ByteString.Lazy           as BL
import           Data.Char                      (chr)
import           Data.List                      as DL (intercalate, stripPrefix)
import           Data.Maybe                     (fromMaybe)
import           GHC.Int
import qualified GHC.Word                       as W
import           Network.Socket
import           Network.Socket.ByteString      (sendAll)
import qualified Network.Socket.ByteString.Lazy as BSL (recv)
import           Network.URI                    hiding (scheme, path)
import           System.Environment

createSocket :: String -> String -> IO (Socket, AddrInfo)
createSocket scheme host = do
  let hints = defaultHints { addrFlags = [AI_ADDRCONFIG], addrSocketType = Stream }
  addr:_ <- getAddrInfo (Just hints) (Just host) (Just scheme)
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  return (sock, addr)

stripSuffix :: String -> String -> Maybe String
stripSuffix suffix str = reverse <$> DL.stripPrefix (reverse suffix) (reverse str)

dropLastColon :: String -> String
dropLastColon = fromMaybe <*> stripSuffix ":"

word8ArrayToString :: [W.Word8] -> String
word8ArrayToString = map (chr . fromIntegral)

-- | Collect data from socket in multiple chunks.  This works, but blocks for
-- awhile when the data received is less than bufferSize.  Also chunk size
-- seems to depend on the server, not always filling the bufferSize.
recvAllChunks :: Socket -> Int64 -> IO [BL.ByteString]
recvAllChunks sock bufferSize = do
  chunk <- BSL.recv sock bufferSize
  -- putStrLn $ "chunk size: " ++ (show $ BL.length chunk)
  if BL.length chunk == 0
    then return [chunk]
    else do
      chunks <- recvAllChunks sock bufferSize
      return (chunk : chunks)

-- | Main entry point and future location of event handler.
main :: IO ()
main = do
  getArgs >>= \case
    [] -> putStrLn "No URL specified"
    url:_ -> do
      putStrLn $ "Navigating to: " ++ url
      let parsedUrl = parseURI url
          scheme = (fromMaybe "Invalid URL" $ (dropLastColon . uriScheme) <$> parsedUrl)
          host = maybe "Invalid URL" uriRegName (uriAuthority =<< parseURI url)
          path = (fromMaybe "Invalid URL" $
                  (('/' :) . DL.intercalate "/" . pathSegments) <$> parsedUrl)
      putStrLn $ "URI Scheme: " ++ scheme
      putStrLn $ "Host: " ++ host
      putStrLn $ "Path: " ++ path

      -- Create network connection to host.
      (sock, addr) <- createSocket scheme host
      connect sock (addrAddress addr)
      let request = "GET " ++ path ++ " HTTP/1.1\r\nHost: " ++ host ++ "\r\n\r\n"
      sendAll sock (BS.pack request)
      response <- recvAllChunks sock 4096
      putStrLn $ word8ArrayToString $ BL.unpack $ mconcat response
      close sock

-- testUrl1 = "http://www.example.com:8080/path/to/page?query=value#fragment"
-- testUrl2 = "http://www.example.com"                           -- small HTTP page
-- testUrl3 = "http://eap.mcgill.ca/MagRack/COG/COG_P_96_05.htm" -- large HTTP page
-- testUrl4 = "http://eap.mcgill.ca/MagRack/COG/cognitio.htm"    -- large HTTP page
-- testUrl5 = "http://reduction.io/essays/rosetta-haskell.html"  -- different chunk size
