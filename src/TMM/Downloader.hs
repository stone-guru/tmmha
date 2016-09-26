-- -*- coding: utf-8 -*-
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables,BangPatterns #-}
module TMM.Downloader(
  ResponseData(..),
  Downloader,
  newDownloader
  )
where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header
import Control.Exception
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.ICU.Convert as ICU  -- text-icu
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.ByteString as B
import qualified Data.Vector as V
import Data.Vector(Vector, (!))
import System.Random
import System.Log.FastLogger

data ResponseData = ResponseData String T.Text (Either T.Text B8.ByteString)| NoData
type Downloader = String -> IO (Response B8.ByteString)

data RoutingContext = RoutingContext{ _manager :: Manager
                                    , _mockAgents :: Vector B.ByteString
                                    , _loggerSet :: LoggerSet
                                    }

newDownloader :: [String] -> IO Downloader
newDownloader agents = do
  context <- RoutingContext <$> newManager tlsManagerSettings
                            <*> return (V.fromList $ map s2b ax)
                            <*> newStdoutLoggerSet defaultBufSize
  return $ download context
  where
    ax |null agents = [defaultAgent]
       |otherwise = agents
    defaultAgent = "Mozilla/5.0 (X11; Linux x86_64; rv:49.0) Gecko/20100101 Firefox/49.0"


download :: RoutingContext -> String -> IO (Response B8.ByteString)
download (RoutingContext manager agents loggerSet) url = do
  req <- parseRequest url >>= addHeaders
  response <- httpLbs req manager
  return response
  where
    {-# INLINE addHeaders #-}
    addHeaders req = do
      agent <- randChoose agents
      return $ req {requestHeaders =
                       [(hUserAgent, agent)
                       ,(hAccept, "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
                       ,(hAcceptLanguage, "zh,en-US;q=0.7,en;q=0.3")
                       ,(hAcceptEncoding, "gzip, deflate, br")
                       ,(hCacheControl, "max-age=0")
                       ,("Upgrade-Insecure-Requests", "1")]}

s2b :: String -> B.ByteString
s2b s = B.pack $ map (fromIntegral . fromEnum) s


randChoose :: Vector a -> IO a  
randChoose v = do
  i <- randomRIO (0, (V.length v) - 1)
  return $ v ! i
