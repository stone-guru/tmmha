-- -*- coding: utf-8 -*-
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module TMM.Downloader(
  CookieUpdater,
  Downloader(..),
  newDownloader,
  download,
  getAgent,
  installCookieUpdater
  )
where

import TMM.Types

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header
  
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.ICU.Convert as ICU  -- text-icu
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.ByteString as B
import Data.Vector(Vector, (!))
import qualified Data.Vector as V
import Data.Time.Clock
  
import System.Random
import Data.Typeable
import Control.Exception
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import Control.DeepSeq

type CookieUpdater = CookieJar -> IO CookieJar

data Downloader = Downloader { _downloader_manager :: Manager
                             , _downloader_mock_agents :: Vector B.ByteString
                             , _downloader_fix_agent :: Maybe Int
                             , _downloader_logger :: Logger
                             , _downloader_cookie :: Maybe (TVar CookieJar)
                             , _downloader_cookie_updater :: Maybe CookieUpdater
                             }

newDownloader :: Bool -> IO Downloader
newDownloader fixAgent = do
  manager <- newManager tlsManagerSettings
  let agents = V.fromList ax
  fixedAgent <- if fixAgent
                then Just <$> randomRIO (0, V.length agents - 1)
                else pure Nothing
  logger <-  initLogger 
  return $ Downloader manager agents fixedAgent logger Nothing Nothing
  where
    ax |null userAgents = [defaultAgent]
       |otherwise = userAgents
  
    defaultAgent = "Mozilla/5.0 (X11; Linux x86_64; rv:49.0) Gecko/20100101 Firefox/49.0"
  
    initLogger = do
      updateGlobalLogger "scraha.spider" (setLevel DEBUG)
      getLogger "scraha.downloader"
      
installCookieUpdater :: Downloader -> CookieJar -> CookieUpdater -> IO Downloader
installCookieUpdater dl c0 updater = do
  cookieVar <- newTVarIO c0
  return $ dl{ _downloader_cookie = Just cookieVar
             , _downloader_cookie_updater = Just updater}

getAgent :: Downloader -> B.ByteString
getAgent downloader = let i = maybe 0 id (_downloader_fix_agent downloader)
                      in (_downloader_mock_agents downloader) ! i


download :: Downloader -> String -> IO (Response B8.ByteString, NominalDiffTime)
download dl url  = timing' $ do
  req <- parseRequest url >>=
         addHeaders >>=
         useCookie (_downloader_cookie dl)
  
  response <- httpLbs req (_downloader_manager dl)
  
  updateCookie (_downloader_cookie dl) req response 
  return response
  where
    {-# INLINE addHeaders #-}
    addHeaders req = do
      agent <- let agents = _downloader_mock_agents dl
                   fixAgent = _downloader_fix_agent dl
               in maybe (randChoose agents) (\i -> return $ agents ! i) fixAgent
      let headers = requestHeaders req
      return $ req {requestHeaders = headers ++
                        [(hUserAgent, agent)
                       ,(hAccept, "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
                       ,(hAcceptLanguage, "zh,en-US;q=0.7,en;q=0.3")
                       ,(hAcceptEncoding, "gzip, deflate, br")
                       ,(hCacheControl, "max-age=0")
                       ,("Upgrade-Insecure-Requests", "1")]}
  
    useCookie Nothing req0 = return req0
    useCookie (Just cookieVar) req0 = do
      c0 <- atomically (readTVar cookieVar)
      c <- case _downloader_cookie_updater dl of
             Nothing -> return c0
             Just f -> f c0
      now <- getCurrentTime
      let (req, newCookie) = insertCookiesIntoRequest req0 c now
      atomically $ writeTVar cookieVar c
      return req

    updateCookie Nothing _ _ = return ()
    updateCookie (Just cookieVar) req resp  = do
      now <- getCurrentTime
      atomically $ modifyTVar cookieVar $ fst . (updateCookieJar resp req now)
   
      
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
       
