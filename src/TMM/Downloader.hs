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
import System.Log.Logger

data ResponseData = ResponseData String T.Text (Either T.Text B8.ByteString)| NoData
type Downloader = String -> IO (Response B8.ByteString)

data RoutingContext = RoutingContext{ _manager :: Manager
                                    , _mockAgents :: Vector B.ByteString
                                    , _loggerSet :: Int --FIXME
                                    }

newDownloader :: IO Downloader
newDownloader = do
  context <- RoutingContext <$> newManager tlsManagerSettings
                            <*> return (V.fromList $ ax)
                            <*> (return 7788) -- FIXME
  return $ download context
  where
    ax |null userAgents = [defaultAgent]
       |otherwise = userAgents
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

userAgents :: [B.ByteString]
userAgents = [ "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.1 "  
             , "(KHTML, like Gecko) Chrome/22.0.1207.1 Safari/537.1"  
             , "Mozilla/5.0 (X11; CrOS i686 2268.111.0) AppleWebKit/536.11 "  
             , "(KHTML, like Gecko) Chrome/20.0.1132.57 Safari/536.11"  
             , "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/536.6 "  
             , "(KHTML, like Gecko) Chrome/20.0.1092.0 Safari/536.6"  
             , "Mozilla/5.0 (Windows NT 6.2) AppleWebKit/536.6 "  
             , "(KHTML, like Gecko) Chrome/20.0.1090.0 Safari/536.6"  
             , "Mozilla/5.0 (Windows NT 6.2; WOW64) AppleWebKit/537.1 "  
             , "(KHTML, like Gecko) Chrome/19.77.34.5 Safari/537.1"  
             , "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/536.5 "  
             , "(KHTML, like Gecko) Chrome/19.0.1084.9 Safari/536.5"  
             , "Mozilla/5.0 (Windows NT 6.0) AppleWebKit/536.5 "  
             , "(KHTML, like Gecko) Chrome/19.0.1084.36 Safari/536.5"  
             , "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/536.3 "  
             , "(KHTML, like Gecko) Chrome/19.0.1063.0 Safari/536.3"  
             , "Mozilla/5.0 (Windows NT 5.1) AppleWebKit/536.3 "  
             , "(KHTML, like Gecko) Chrome/19.0.1063.0 Safari/536.3"  
             , "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_0) AppleWebKit/536.3 "  
             , "(KHTML, like Gecko) Chrome/19.0.1063.0 Safari/536.3"  
             , "Mozilla/5.0 (Windows NT 6.2) AppleWebKit/536.3 "  
             , "(KHTML, like Gecko) Chrome/19.0.1062.0 Safari/536.3"  
             , "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/536.3 "  
             , "(KHTML, like Gecko) Chrome/19.0.1062.0 Safari/536.3"  
             , "Mozilla/5.0 (Windows NT 6.2) AppleWebKit/536.3 "  
             , "(KHTML, like Gecko) Chrome/19.0.1061.1 Safari/536.3"  
             , "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/536.3 "  
             , "(KHTML, like Gecko) Chrome/19.0.1061.1 Safari/536.3"  
             , "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/536.3 "  
             , "(KHTML, like Gecko) Chrome/19.0.1061.1 Safari/536.3"  
             , "Mozilla/5.0 (Windows NT 6.2) AppleWebKit/536.3 "  
             , "(KHTML, like Gecko) Chrome/19.0.1061.0 Safari/536.3"  
             , "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.24 "  
             , "(KHTML, like Gecko) Chrome/19.0.1055.1 Safari/535.24"  
             , "Mozilla/5.0 (Windows NT 6.2; WOW64) AppleWebKit/535.24 "  
             , "(KHTML, like Gecko) Chrome/19.0.1055.1 Safari/535.24" ]
       
