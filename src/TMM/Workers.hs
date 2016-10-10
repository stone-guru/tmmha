-- -*- coding: utf-8 -*-
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TMM.Workers(
  Spider(),
  Summary(..),
  Config(..),
  newSpider,
  startSpider,
  waitStarted,
  isIdle,
  waitIdle,
  waitEnd,
  askClose,
  waitClosed,
  summary,
  executeAction,
  defaultConfig,
  transmitParser
  )where

import TMM.Types
import qualified TMM.Downloader as DL
  
import System.IO (stderr)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.MVar
import qualified Data.Text.IO as T
import qualified Data.List as L
import Control.Exception
import Control.DeepSeq
import qualified Data.HashMap.Strict as M
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.ICU.Convert as ICU  -- text-icu
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import Network.HTTP.Client
import Network.HTTP.Types.Header
import Data.Maybe
import Text.HTML.TagSoup
import qualified Text.HTML.TagSoup.Fast as F
import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import Debug.Trace

class Octopus a where
  newInstance :: ScraParser -> ScraProcessor -> IO a
  isInstIdle :: a -> IO Bool
  waitIntClosed :: a -> IO ()

--
type ScraResponse = Response LB8.ByteString
data ResponseData = ResponseData TaskDesc ScraResponse

instance NFData ResponseData where
  rnf (ResponseData t resp) = rnf t

type TaskQueue = TChan (Maybe TaskData)
type ResponseQueue = TChan (Maybe ResponseData)

type PilotLight = TVar Bool

data Config = Config { _cParsers :: [(Text, ScraParser, DataFormat)] -- name, parser, format
                     , _cProcessors :: [(Text ,ScraProcessor)] -- processor name, processor
                     , _cDownThreadNum :: Int
                     }
  
data Spider = Spider { _taskQueue :: TaskQueue
                     , _taskLight :: PilotLight
                     , _respQueue :: ResponseQueue
                     , _respLight :: PilotLight
                     , _parserMap :: M.HashMap Text ParserRec
                     , _processorMap :: M.HashMap Text ProcessorRec
                     , _startFlag :: MVar Bool
                     , _finishFlag :: MVar Bool
                     , _downloadCount :: TVar Int
                     , _downloadBytes :: TVar Int
                     , _nDownloadThread :: Int
                     , _logger :: Logger
                     }

data ParserRec = ParserRec { _parName :: Text
                           , _parExecute ::  ScraParser
                           , _parFmt :: DataFormat
                           , _parNumOfTimes :: TVar Int
                           , _parElapsed :: TVar Int
                           }

data ProcessorRec = ProcessorRec { _prcName :: Text
                                 , _prcExecute :: ScraProcessor
                                 , _prcNumOfTimes :: TVar Int
                                 , _prcElapsed :: TVar Int
                                 }

data Summary = Summary { _sDownloadCount :: Int
                       , _sDownloadBytes :: Int
                       }deriving(Show)

executeAction :: Config
              -> StartsUrls -- start crawling from these urls
              -> IO Summary -- summary of actions
executeAction config urls = do
  spider <- newSpider config
  startSpider spider urls
   -- assure begin work
  waitStarted spider
  -- wait until finish works
  waitEnd spider 
  askClose spider
  waitClosed spider
  
  summary spider

defaultConfig :: Config
defaultConfig = Config [] [] 1

newSpider :: Config -> IO Spider
newSpider (Config pars prcs ndt) = do
  startFlag <- newEmptyMVar
  finishFlag <- newEmptyMVar
  logger <- initLogger 
  atomically $ do
    taskq <- newTChan
    respq <- newTChan
    tpl <- newTVar False
    rpl <- newTVar False
    parMap <- createParserMap pars
    prcMap <- createProcessorMap prcs
    dc <- newTVar 0
    db <- newTVar 0
    return $ Spider taskq tpl respq rpl parMap prcMap
                    startFlag finishFlag dc db (within 1 4 ndt) logger
  where
    createParserMap pars = do
      pdx <- mapM (\(pn, parser, df) -> ParserRec pn parser df <$> newTVar 0 <*> newTVar 0) pars
      return $ M.fromList $ map (\d -> (_parName d, d)) pdx
  
    createProcessorMap prcs = do
      pdx <- mapM (\(pn, processor) -> ProcessorRec pn processor <$> newTVar 0 <*> newTVar 0) prcs
      return $ M.fromList $ map (\d -> (_prcName d, d)) pdx

    initLogger = do
      s <- openlog "SyslogStuff" [PID] USER DEBUG
      updateGlobalLogger "scraha.spider" (addHandler s)
      updateGlobalLogger "scraha.spider" (setLevel DEBUG)
      getLogger "scraha.spider"

    within a b x
      |x < a = a
      |x > b = b
      |otherwise = x
      
startSpider :: Spider -> StartsUrls -> IO ()
startSpider spider urls = do
  fdownload <- DL.newDownloader
  flip mapM [1.._nDownloadThread spider] $ \i ->
    let name = "downloader" ++ show i
    in  forkIO (downloadRoute spider fdownload name)
    
  forkIO (processRoute spider >> putMVar (_finishFlag spider) True) --FIXME: error handling
  info spider "Scraha spider start"
  
  atomically $ mapM_  feedStartsUrl urls
  putMVar (_startFlag spider) True
  where
    feedStartsUrl (url, parName) = writeTChan (_taskQueue spider) $
                                        Just $ TaskData $ TaskDesc url  M.empty parName

waitStarted :: Spider -> IO ()
waitStarted = void . readMVar . _startFlag

downloadRoute :: Spider -> DL.Downloader -> String -> IO ()
downloadRoute spider dl rn = mapChannel rn (_taskQueue spider) (_taskLight spider)
                             (_respQueue spider) download
  where
    download (TaskData td) = do
      let url = T.unpack $ _tdUrl td
      i <- atomically $ readTVar (_downloadCount spider)
      info spider $ show i ++ ", download.. " ++ show td
      -- core action
      response <- dl url

      let nbytes :: Int = fromIntegral $ LB8.length (responseBody response)
      info spider $ lensMsg i nbytes
  
      atomically $ do
        modifyTVar (_downloadCount spider) (+1)
        modifyTVar (_downloadBytes spider) (+ nbytes)
  
      return [ResponseData td response]
    lensMsg i n = show i ++ ", downloaded " ++ show n ++ " bytes"

summary :: Spider -> IO Summary
summary s = atomically $ do
  dc <- readTVar $ _downloadCount s
  db <- readTVar $ _downloadBytes s
  return $ Summary dc db
  
spiderLog :: Priority -> Spider -> String -> IO ()
spiderLog p spider s = logL (_logger spider) p s

debug = spiderLog DEBUG
info = spiderLog INFO
err = spiderLog ERROR

getParser :: Spider -> Text -> ParserRec
getParser spider hn = case M.lookup hn (_parserMap spider) of
  Just par -> par
  _ -> error $ "no parser named " ++ T.unpack hn

getProcessor :: Spider -> Text -> ProcessorRec
getProcessor spider hn = case M.lookup hn (_processorMap spider) of
  Just prs -> prs
  _ -> error $ "no processor named " ++ T.unpack hn

analysisResp :: Spider -> TaskDesc -> ScraResponse -> IO (Maybe SourceData)
analysisResp spider td response = do
  case M.lookup (_tdHandler td) (_parserMap spider) of
    Nothing -> return Nothing
    Just pr -> do
      case _parFmt pr of
        RawBinary -> return . Just $ SourceData td mime (OBinary bytes)
        UtfText -> do
          txt <- unicoding charset bytes
          return . Just $ SourceData td mime (OText txt)
        Tags -> do
          let charset_ = charset
          trace ("**" ++ charset ++ ", " ++ show mime) $ return () 
          cvt <-  ICU.open charset Nothing
          let b2t = ICU.toUnicode cvt
          let tags = map (textTag b2t) $ F.parseTags bytes
          return . Just $ SourceData td mime (OTags tags)
      
  where
    bytes :: B8.ByteString = LB8.toStrict $ responseBody response

    charset :: String = case lookup hContentType (responseHeaders response) of
                          Nothing -> "utf-8"
                          Just ct -> let rest = snd (B.breakSubstring "=" ct)
                                     in if B.null rest then "utf-8"
                                        else b2s $ B.tail rest
                            
    mime :: Text = case lookup hContentType (responseHeaders response) of
                     Nothing -> "application/octet-stream"
                     Just ct -> E.decodeUtf8 $ fst (B.breakSubstring ";" ct)
                        
    unicoding :: String -> B.ByteString -> IO Text
    unicoding charset bytes = do
      locale <- ICU.open charset Nothing
      let txt = ICU.toUnicode locale bytes
      return $! txt

type B2T = B8.ByteString -> Text

textTag :: B2T -> Tag B8.ByteString -> Tag Text
textTag bst (TagOpen t a) = TagOpen (bst t) [(bst n, bst v) | (n,v) <- a]
textTag bst (TagClose t) = TagClose (bst t)
textTag bst (TagText t) = TagText (bst t)
textTag bst (TagComment t) = TagComment (bst t)
textTag bst (TagWarning t) = TagWarning (bst t)
textTag bst (TagPosition r c) = TagPosition r c

processRoute :: Spider -> IO ()
processRoute spider = mapChannel "process" (_respQueue spider) (_respLight spider)
                         (_taskQueue spider) process
  where
    process (ResponseData td resp) = do
      
      od_ <- analysisResp spider td resp
      case od_ of
        Just od -> do
          let pr = getParser spider (_tdHandler td)
          ydx <- _parExecute pr od
          lst <- mapM dispatch ydx
          return $ catMaybes lst
        Nothing -> do
          err spider $ "No parser found named " ++ T.unpack (_tdHandler td)
          return []

    dispatch ::  YieldData -> IO (Maybe TaskData)
    dispatch (YieldData _ (YUrl td))  = return $ Just $ TaskData td
    dispatch yd = do
        let prcRec = getProcessor spider (_tdHandler $_ydDesc yd)
        _prcExecute prcRec (yd2rd yd)
        return Nothing

    yd2rd (YieldData td ent) = ResultData td $ ye2re ent
    ye2re (YBinary b) = RBinary b
    ye2re (YText t) = RText t
    ye2re (YJson j) = RJson j
    ye2re _ = error "unhandled YieldData Type"

transmitParser :: SourceData -> IO [YieldData]
transmitParser (SourceData td _ se) = return [YieldData td (fromSrc se)]
  where
    fromSrc (OBinary bx) = YBinary bx
    fromSrc (OText txt) = YText txt
    fromSrc _ = error "transmitParser accept binary and text only"

isIdle :: Spider -> IO Bool
isIdle s  = do
  tb <- isTurnedOn "downloader" (_taskLight s)
  if tb
    then return False
    else fmap not (isTurnedOn "processor" $ _respLight s)

waitIdle :: Spider -> Int -> IO ()
waitIdle s ms = isIdle s >>= \b ->
  unless b $ do
    threadDelay ms
    waitIdle s ms

waitEnd :: Spider -> IO ()
waitEnd spider = do
  repeatDo "wait end"$ do
    threadDelay $ 1000 * 1000
    b <- isIdle spider
    return $ not b

askClose :: Spider -> IO ()
askClose s = atomically $ void $ forM_  (replicate 24  Nothing) (writeTChan (_taskQueue s))

waitClosed :: Spider -> IO ()
waitClosed  =  void . takeMVar . _finishFlag

mapChannel :: (NFData a, NFData b) =>  String -> TChan (Maybe a) -> PilotLight  ->
              TChan (Maybe b) -> (a -> IO [b]) -> IO ()
mapChannel msg cha pla chb f =
  repeatDo msg $ bracket
  (do
      --putStrLn $ msg ++ " waiting next"
      atomically $ readTChan cha)
  (\_ -> do
      --putStrLn $ msg ++ "turn Off"
      turnOff pla)
  (\x_ -> do
      turnOn pla
      --putStrLn $ msg ++ " got one"
      --putStrLn $ msg ++ " turn On"
      case x_ of
        Nothing -> do
          atomically $ writeTChan chb Nothing
          return False
        Just x -> do
          yx_ <- f x
          yx <- evaluate $ force yx_
          atomically $ mapM_ (writeTChan chb . Just)  yx
          return True)

turnOn :: PilotLight -> IO ()
turnOn t = switch t True

turnOff :: PilotLight -> IO ()
turnOff t = switch t False

isTurnedOn :: String -> PilotLight -> IO Bool
isTurnedOn msg mv = readTVarIO mv
--  b <- takeMVar mv
--  putMVar mv b
  -- putStrLn $ "check " ++ msg ++ " lighter is" ++ show b
--  return b

switch :: PilotLight -> Bool -> IO ()
switch mv b = atomically $ writeTVar mv b

repeatDo :: String -> IO Bool -> IO ()
repeatDo msg f = do
  b <- f
  if not b
    then putStrLn $ msg ++ " closed"
    else repeatDo msg f

b2s :: B.ByteString -> String
b2s = map (toEnum . fromIntegral) . B.unpack
