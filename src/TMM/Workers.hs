-- -*- coding: utf-8 -*-
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module TMM.Workers(
  Spider(),
  Summary(..),
  Config(..),
  CookieUpdater,
  newSpider,
  startSpider,
  waitStarted,
  isIdle,
  waitEnd,
  askClose,
  waitClosed,
  summary,
  executeAction,
  defaultConfig,
  transmitParser,
  )where

import TMM.Types
import TMM.Downloader(CookieUpdater, Downloader)
import qualified TMM.Downloader as DL

import System.IO (stderr)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
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
import Data.Either
import Text.HTML.TagSoup
import qualified Text.HTML.TagSoup.Fast as F
import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import Debug.Trace
import Data.Typeable
import Data.Time.Clock

class Octopus a where
  newInstance :: ScraParser -> ScraProcessor -> IO a
  isInstIdle :: a -> IO Bool
  waitIntClosed :: a -> IO ()

--

data SessionData = forall hs. Typeable hs => SessionData hs

type ScraResponse = Response LB8.ByteString
data ResponseData = ResponseData TaskDesc ScraResponse

instance NFData ResponseData where
  rnf (ResponseData t resp) = rnf t

data Channel a = Channel { _channel_name :: String
                         , _channel_tchan :: TChan (Either String a)
                         , _channel_elem_num :: TVar Int
                         , _channel_task_running :: TVar Int
                         , _channel_closed :: TVar Bool
                         }


type TaskQueue = Channel TaskData

type ResponseQueue = Channel ResponseData


data CookieAction = forall hs. Typeable hs => CookieAction (hs -> CookieJar -> IO CookieJar)

data Config = Config { _cParsers :: [(Text, ScraParser, DataFormat)] -- ^ Name, parser, format
                     , _cProcessors :: [(Text ,ScraProcessor)]       -- ^ Processor name, processor
                     , _cDownThreadNum :: Int                        -- ^ How many download thread
                     , _cDownloadInterval :: Int                     -- ^ Download action interval
                     , _cEnableCookie :: Bool
                     , _cInitSession :: IO (Maybe (CookieJar, SessionData))
                     , _cCookieAction :: Maybe CookieAction
                     , _cFixAgent :: Bool
                     }

data Spider = Spider { _taskQueue :: TaskQueue
                     , _respQueue :: ResponseQueue
                     , _parserMap :: M.HashMap Text ParserRec
                     , _processorMap :: M.HashMap Text ProcessorRec
                     , _startFlag :: MVar Bool
                     , _finishFlag :: MVar Bool
                     , _downloadCount :: TVar Int
                     , _downloadBytes :: TVar Int
                     , _logger :: Logger
                     , _config :: Config
                     , _sessionData :: TVar (Maybe SessionData)
                     }

data ParserRec = ParserRec { _parName :: Text
                           , _parExecute ::  ScraParser
                           , _parFmt :: DataFormat
                           , _parExecStat :: TVar ExecStat
                           }

data ProcessorRec = ProcessorRec { _prcName :: Text
                                 , _prcExecute :: ScraProcessor
                                 , _prcExecStat :: TVar ExecStat
                                 }

data Summary = Summary { _sDownloadCount :: Int
                       , _sDownloadBytes :: Int
                       , _sParExecStat :: [(Text, ExecStat)]
                       , _sPrcExecStat :: [(Text, ExecStat)]
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
defaultConfig = Config [] --parsers
                [] --processors
                1 -- num of download thread
                500 -- ms interval between download
                False -- enable cookie
                (return Nothing) -- session initialial function
                Nothing -- cookie action
                False -- use fixed agent in request headers

newSpider :: Config -> IO Spider
newSpider config@(Config pars prcs ndt downInter _ _ _ _) = do
  startFlag <- newEmptyMVar
  finishFlag <- newEmptyMVar
  logger <- initLogger
  atomically $ do
    taskq <- newChannel "download task queue"
    respq <- newChannel "response data queue"
    parMap <- createParserMap pars
    prcMap <- createProcessorMap prcs
    dc <- newTVar 0
    db <- newTVar 0
    sd <- newTVar Nothing
    return $ Spider taskq respq parMap prcMap
                    startFlag finishFlag dc db
                    logger config sd

  where
    createParserMap pars = do
      pdx <- mapM (\(pn, parser, df) -> ParserRec pn parser df <$> newTVar execStat0) pars
      return $ M.fromList $ map (\d -> (_parName d, d)) pdx

    createProcessorMap prcs = do
      pdx <- mapM (\(pn, processor) -> ProcessorRec pn processor <$> newTVar execStat0) prcs
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
  downloader <- DL.newDownloader (_cFixAgent config)
                >>= installSessionHandler

  forM_ [1 .. _cDownThreadNum config] $
    \i -> let name = "downloader" ++ show i
          in  forkIO (downloadRoute spider downloader name)

  forkIO (processRoute spider >> putMVar (_finishFlag spider) True) --FIXME: error handling
  info spider "Scraha spider start"

  atomically $ mapM_  feedStartsUrl urls
  putMVar (_startFlag spider) True
  where
    config = _config spider
    feedStartsUrl (url, meta, parName) = writeChannel (_taskQueue spider) $
                                            TaskData $ TaskDesc url meta parName

    installSessionHandler downloader
      |  _cEnableCookie config =  do
           sd_ <- _cInitSession config
           case sd_ of
             Just (cookieJar, hs) -> do
               atomically $ writeTVar (_sessionData spider) (Just hs)
               DL.installCookieUpdater downloader cookieJar updateCookieBySession
             Nothing -> return downloader
      |otherwise = return downloader

    updateCookieBySession :: CookieJar -> IO CookieJar
    updateCookieBySession cookie = do
      sd_ <- atomically $ readTVar (_sessionData spider)
      case sd_ of
        Nothing -> return cookie
        Just (SessionData ses) -> case _cCookieAction config of
          Nothing -> return cookie
          Just (CookieAction f) -> case cast ses of
                                     Just ses' -> f ses' cookie
                                     Nothing -> castError
    castError = let s = "when updateCookie can not cast stored session to fit CookieAction"
                in err spider s >> error s

waitStarted :: Spider -> IO ()
waitStarted = void . readMVar . _startFlag

downloadRoute :: Spider -> DL.Downloader -> String -> IO ()
downloadRoute spider dl rn = mapChannel rn (_logger spider)
                             (_taskQueue spider) (_respQueue spider)
                             download interVal 
  where
    interVal = _cDownloadInterval $ _config spider
    download (TaskData td) = do
      let url = T.unpack $ _tdUrl td
      i <- atomically $ readTVar (_downloadCount spider)
      info spider $ show i ++ ", download.. " ++ show td
      -- core action
      (response, dt) <- DL.download dl url
      
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
  parStatx <- readStatx _parName _parExecStat  (M.elems $ _parserMap s)
  prcStatx <- readStatx  _prcName _prcExecStat (M.elems $ _processorMap s)
  return $ Summary dc db parStatx prcStatx
  where
    readStatx nf sf stx = mapM (readStat nf sf) stx
    readStat nf sf r = (,) (nf r) <$> readTVar (sf r)

spiderLog :: Priority -> Spider -> String -> IO ()
spiderLog p spider s = logL (_logger spider) p s

debug = spiderLog DEBUG
info = spiderLog INFO
err = spiderLog ERROR

getParser :: Spider -> Text -> ParserRec
getParser spider hn
  |Just par <-  M.lookup hn (_parserMap spider) = par
  |otherwise = error $ "no parser named " ++ T.unpack hn

getProcessor :: Spider -> Text -> ProcessorRec
getProcessor spider hn = case M.lookup hn (_processorMap spider) of
  Just prs -> prs
  _ -> error $ "no processor named " ++ T.unpack hn

convertResp :: Spider -> TaskDesc -> ScraResponse -> IO SourceData
convertResp spider td response = convertTo $ _parFmt $ getParser spider (_tdHandler td)
  where
    convertTo RawBinary = do
      return $ SourceData td mime (OBinary bytes)
    convertTo UtfText =  do
      txt <- unicoding charset bytes
      return $ SourceData td mime (OText txt)
    convertTo Tags = do
      cvt <-  ICU.open charset Nothing
      let b2t = ICU.toUnicode cvt
      let tags = map (textTag b2t) $ F.parseTags bytes -- FIXME use GHC Rule to opt
      return $ SourceData td mime (OTags tags)
    
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
processRoute spider = mapChannel "process" (_logger spider)
                      (_respQueue spider) (_taskQueue spider) process 0 
  where
    process (ResponseData td resp) = do
      od <- convertResp spider td resp
      let pr = getParser spider (_tdHandler td)        
      (ydx, dt) <- timing $! _parExecute pr od    -- run parser
      atomically $ modifyTVar (_parExecStat pr) (addExecRec dt)

      lst <- mapM dispatch ydx
      return $ catMaybes lst

    dispatch ::  YieldData -> IO (Maybe TaskData)
    dispatch (YieldData _ (YUrl td))  = return $ Just $ TaskData td
    dispatch yd = do
        let prc = getProcessor spider (_tdHandler $_ydDesc yd)
        ((), dt) <- timing $! _prcExecute prc (yd2rd yd)
        atomically $ modifyTVar (_prcExecStat prc) (addExecRec dt)
        return Nothing

    yd2rd (YieldData td ent) = ResultData td $ ye2re ent
    ye2re (YBinary b) = RBinary b
    ye2re (YText t) = RText t
    ye2re (YJson j) = RJson j
    ye2re (YData x) = RData x
    ye2re _ = error "unhandled YieldData Type"

transmitParser :: SourceData -> IO [YieldData]
transmitParser (SourceData td _ se) = return [YieldData td (fromSrc se)]
  where
    fromSrc (OBinary bx) = YBinary bx
    fromSrc (OText txt) = YText txt
    fromSrc _ = error "transmitParser accept binary and text only"

isIdle :: Spider -> IO Bool
isIdle s = atomically $ do
  b1 <- isChannelBusy (_taskQueue s)
  if b1
     then return False
     else not <$> isChannelBusy (_respQueue s)

waitEnd :: Spider -> IO ()
waitEnd spider = repeatDo "waiting function" 1000 $ do
  b <- isIdle spider
  -- taskqStatus <- atomically $ channelStatus (_taskQueue spider)
  -- print taskqStatus
  -- putStrLn $ "spider idle is " ++ show b
  return $ not b

askClose :: Spider -> IO ()
askClose s = atomically $ forM_  (replicate 24 "finished") (writeMsgEnd (_taskQueue s))

waitClosed :: Spider -> IO ()
waitClosed  =  void . takeMVar . _finishFlag

repeatDo :: String -> Int -> IO Bool -> IO ()
repeatDo name interval f =  loop >> putStrLn (name ++ " end ")
  where
    loop = do
      continue <- f
      if not continue
        then return ()
        else do
          when (interval > 0) $ threadDelay (interval * 1000)
          loop

b2s :: B.ByteString -> String
b2s = map (toEnum . fromIntegral) . B.unpack

-- for channel
  
newChannel :: String -> STM (Channel a)
newChannel name = Channel name <$> newTChan <*>  newTVar 0 <*> newTVar 0 <*> newTVar False

writeChannel :: Channel a -> a -> STM ()
writeChannel chan a = do
  closed <- isChannelClosed chan
  unless closed $ do
    writeTChan (_channel_tchan chan) (Right a)
    modifyTVar (_channel_elem_num chan) (+1)

writeMsgEnd :: Channel a -> String -> STM ()
writeMsgEnd chan s = writeTChan (_channel_tchan chan) (Left s)

readChannel :: Channel a -> STM (Either String a)
readChannel chan = do
  closed <- isChannelClosed chan
  if closed
    then return $ Left (_channel_name chan ++ " was closed!")
    else do
      a <- readTChan (_channel_tchan chan)
      when (isRight a) $ do
        modifyTVar (_channel_elem_num chan) (\x -> x - 1)
        modifyTVar (_channel_task_running chan) (+1)
      return a

closeChannel :: Channel a -> STM ()
closeChannel chan = writeTVar (_channel_closed chan) True

channelTaskEnd :: Channel a -> STM ()
channelTaskEnd chan = modifyTVar  (_channel_task_running chan) (\x -> x - 1)

isChannelBusy :: Channel a -> STM Bool
isChannelBusy chan = do
  nElem <- readTVar (_channel_elem_num chan)
  if nElem > 0
    then return True
    else do
      nTask <- readTVar (_channel_task_running chan)
      return $ nTask > 0

channelStatus :: Channel a -> STM (String, Bool, Int, Int)
channelStatus cha = (,,,)  (_channel_name cha)
                      <$> readTVar (_channel_closed cha)
                      <*> readTVar (_channel_elem_num cha)
                      <*> readTVar (_channel_task_running cha)

isChannelClosed :: Channel a -> STM Bool
isChannelClosed chan = readTVar (_channel_closed chan)

mapChannel :: (NFData a, NFData b)
           => String -> Logger -> Channel a -> Channel b -> (a -> IO [b]) -> Int -> IO ()
mapChannel routeName logger cha chb f interval =
  repeatDo routeName interval $
    bracket (atomically $ readChannel cha)
            (\_ -> atomically $ channelTaskEnd cha)
            process
  where
    process (Left s) = chainedClose s
    process (Right x) = do
      r <- try $ f x
      case r of
        Right yx_ -> do
          yx <- evaluate $ force yx_
          atomically $ mapM_ (writeChannel chb) yx
          return True
        Left (e::SomeException) -> chainedClose $ show e
  
    chainedClose msg = do
      let prior = if msg == "finished" then INFO else ERROR
      logL logger prior $ routeName ++ " end by msg: " ++ msg
      atomically $ writeMsgEnd chb msg
      return False

data ExecStat = ExecStat Int NominalDiffTime deriving Show

execStat0 :: ExecStat
execStat0 = ExecStat 0 (toEnum 0)

addExecRec :: NominalDiffTime -> ExecStat -> ExecStat
addExecRec dt2 (ExecStat n dt1) = ExecStat (n + 1) (dt1 + dt2)

