-- -*- coding: utf-8 -*-
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module TMM.Downloader where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header
import Control.Exception
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.ICU.Convert as ICU  -- text-icu
import qualified Data.Text.ICU as ICU
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.ByteString as B
import qualified Data.Vector as V
import Data.Vector(Vector, (!))
import System.Random
import System.Log.FastLogger

data Downloader = Downloader{ _manager :: Manager
                            , _mockAgents :: Vector B.ByteString
                            , _loggerSet :: LoggerSet
                            }
data ResponseData = TextData T.Text T.Text | BinaryData T.Text B8.ByteString | NoData

newDownloader :: [String] -> IO Downloader
newDownloader agents = 
  Downloader <$> newManager tlsManagerSettings
             <*> return (V.fromList $ map s2b ax)
             <*> newStdoutLoggerSet defaultBufSize
  where
    {-# INLINE s2b #-}
    s2b s = B.pack $ map (fromIntegral . fromEnum) s
    ax |null agents = [defaultAgent]
       |otherwise = agents
    defaultAgent = "Mozilla/5.0 (X11; Linux x86_64; rv:49.0) Gecko/20100101 Firefox/49.0"
  


download :: Downloader -> String -> IO ResponseData
download (Downloader manager agents loggerSet) url = do
  req_ <- parseRequest url
  reqHeaders <- genHeaders
  let req = req_ {requestHeaders = reqHeaders}
  
  response <- httpLbs req manager
  
  let headers = responseHeaders  response
  case parseContentType $ lookup hContentType headers of
    (False, Just ctype, _) -> do
      return $ BinaryData ctype (responseBody response)
      
    (True, Just ctype, charset_) -> do
      let charset = maybe "utf-8" id charset_
      txt <- unicoding charset $ responseBody response
      return $ TextData ctype txt
      
  where
    {-# INLINE parseContentType #-}
    parseContentType :: Maybe B.ByteString -> (Bool, Maybe T.Text, Maybe String)
    parseContentType Nothing =
      (False, Just "application/octet-stream", Nothing)
    parseContentType (Just ct) =
      let isText = (B.isPrefixOf "text/" ct) || (B.isPrefixOf "application/js" ct)
          contentType = fst $ B.breakSubstring ";" ct
          charset = let rest = snd (B.breakSubstring "=" ct)
                    in if B.null rest then Nothing else Just $ b2s (B.tail rest)
      in (isText, Just (E.decodeUtf8 contentType), charset)

    {-# INLINE b2s #-}
    b2s = map (toEnum . fromIntegral) . B.unpack
  
    {-# INLINE genHeaders #-}
    genHeaders = do
      agent <- randChoose agents
      return $ [(hUserAgent, agent)
               ,(hAccept, "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
               ,(hAcceptLanguage, "zh,en-US;q=0.7,en;q=0.3")
               ,(hAcceptEncoding, "gzip, deflate, br")
               ,(hCacheControl, "max-age=0")
               ,("Upgrade-Insecure-Requests", "1")]

    unicoding charset bytes = do
      locale <- ICU.open charset Nothing
      let txt = ICU.toUnicode locale $ B8.toStrict bytes
      return txt
      
    {-# INLINE randChoose #-}
    randChoose v = do
      i <- randomRIO (0, (V.length v) - 1)
      return $ v ! i
