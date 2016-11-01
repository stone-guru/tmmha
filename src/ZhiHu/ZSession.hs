-- -*- coding: utf-8 -*-
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ZhiHu.ZSession (
  Xsrf,
  zhiHuLogin,
  updateCookieXsrf
)where

import TMM.Types
-- import TMM.Selector
import TMM.Downloader

import qualified Data.Char as C
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LB
import Text.HTML.TagSoup
import qualified Data.Text.ICU.Convert as ICU  -- text-icu
import qualified Data.Text.ICU as ICU
import qualified Data.Text.ICU.Regex as R
import qualified Data.List as L
import Data.Maybe
import Control.Applicative ((<$>), (<*>))
import Debug.Trace
import qualified Text.HTML.TagSoup.Fast as F
import System.IO
import Control.Monad
import Network.HTTP.Types.Header
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Data.Time.Clock
import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString.Lazy
import Data.Typeable
import qualified Data.HashMap.Strict as M
import qualified Data.Aeson as J

newtype Xsrf = Xsrf {xsrfValue :: B8.ByteString} deriving (Show, Eq, Typeable)

zhiHuLogin :: Downloader -> IO (Either String (CookieJar, SessionData))
zhiHuLogin downloader = do
  (cookieJar0, xsrf) <- processEntryPage downloader
  result <- performLogin downloader cookieJar0 xsrf
  return $  fmap (\cookieJar -> (cookieJar, SessionData xsrf)) result
    
updateCookieXsrf :: Xsrf -> CookieJar -> IO CookieJar
updateCookieXsrf xsrf cookieJar = return $ createCookieJar (loop (destroyCookieJar cookieJar))
  where
    loop [] = []
    loop (ck:ckx)
      |cookie_name ck == "_xsrf" = ck{cookie_value = xsrfValue xsrf} : ckx
      |otherwise = ck : loop ckx

performLogin :: Downloader -> CookieJar -> Xsrf -> IO (Either String CookieJar)
performLogin downloader cookieJar0 xsrf = do
  let url = "https://www.zhihu.com/login/phone_num"
  req <- assembly xsrf  <$> parseRequest url
  now <- getCurrentTime
  let (loginReq, cookieJar1) = insertCookiesIntoRequest req cookieJar0 now
  -- putStrLn $ "cookieJar2 is " ++ show cookieJar2
  -- putStrLn " loging request is:"
  -- putStrLn $ show loginReq
  -- putStrLn "******* loging response ************"
  response <- httpLbs loginReq (_downloader_manager downloader)
  -- putStrLn $ show $ (responseBody response)
  case checkLoginResult (responseBody response) of
    Nothing -> Right <$> updateCookie response loginReq cookieJar1 
    Just s -> return $ Left s
  where
    checkLoginResult :: LB.ByteString -> Maybe String 
    checkLoginResult bytes = case J.decode bytes :: Maybe J.Object of
                               Nothing -> Just "login response unrecogelized"
                               Just obj -> case intValueMaybe "r" obj of
                                 Just 0 -> Nothing
                                 Just i -> Just $ concat ["r = ", show i, " msg=", strValue "msg" obj]
                                 Nothing -> Just $ "can not get result code"
    intValueMaybe fn obj
      |Just (J.Number n) <- M.lookup fn obj = Just $ truncate n
      |otherwise = Nothing

    strValue fn obj
      |Just (J.String s) <- M.lookup fn obj = T.unpack s
      |otherwise = ""
      
    updateCookie response  req cookieJar = do
      now <- getCurrentTime
      let (cookieJar2, _) = updateCookieJar response req now cookieJar
      return cookieJar2

    assembly xsrf req = let headers = requestHeaders req
                        in req { method="POST"
                               , requestBody=RequestBodyLBS $ postForm
                               , requestHeaders =
                                   headers ++
                                   [(hUserAgent, getAgent downloader)
                                   ,(hAccept, "*/*")
                                   ,(hAcceptLanguage, "zh,en-US;q=0.7,en;q=0.3")
                                   ,(hAcceptEncoding, "gzip, deflate, br")
                                   , ("Referer", "https://www.zhihu.com/")
                                   , ("X-Requested-With", "XMLHttpRequest")
                                   , ("X-Xsrftoken", xsrfValue xsrf)
                                   , ("Content-Type",
                                       "application/x-www-form-urlencoded; charset=UTF-8")
                                   ]}
    postForm = LB.concat ["_xsrf=", LB.fromStrict $ xsrfValue xsrf ,
                          "&password=timeismoney",
                           "&captcha_type=cn",
                           "&remember_me=true",
                           "&phone_num=13880281637"]

processEntryPage :: Downloader -> IO (CookieJar, Xsrf)
processEntryPage downloader = do
  req <- addHeaders <$> parseRequest "https://www.zhihu.com"
  resp <- httpLbs req (_downloader_manager downloader)
  -- putStrLn $ "response1 content"
  -- putStrLn $ show resp1
  let cookieJar = responseCookieJar resp
  putStrLn $ "cookie of entry-page response"
  putStrLn $ show cookieJar
  let xsrf = parseXsrf (responseBody resp)
  putStrLn $ "xsrf = " ++ show xsrf
  return (cookieJar, Xsrf xsrf)
  where
    addHeaders req = let headers = requestHeaders req
                     in req {requestHeaders =
                                headers ++
                                [(hUserAgent, getAgent downloader)
                                ,(hAccept, "*/*")
                                ,(hAcceptLanguage, "zh,en-US;q=0.7,en;q=0.3")
                                ,(hAcceptEncoding, "gzip, deflate, br")
                                ]}

parseXsrf :: LB.ByteString -> B8.ByteString
parseXsrf bytes = match $ bytes =~ ("name=\"_xsrf\" +value=\"([^\"]*)" :: LB.ByteString)
  where
    match :: (LB.ByteString, LB.ByteString, LB.ByteString, [LB.ByteString]) -> B8.ByteString
    match (_, _, _, [x]) = LB.toStrict x
