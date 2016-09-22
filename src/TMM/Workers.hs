-- -*- coding: utf-8 -*-
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module TMM.Workers where

import TMM.Types
import qualified TMM.Downloader as DL
import Control.Concurrent
import Control.Concurrent.Chan
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.ICU.Convert as ICU  -- text-icu
import qualified Data.Text.ICU as ICU
import Control.Exception
import qualified Data.ByteString.Lazy.Char8 as LB
import Network.HTTP.Client
import Network.HTTP.Client.TLS

data Lighter = Lighter (MVar Bool)
  
data Spider = Spider { _taskQueue :: TaskQueue
                     , _taskLighter :: Lighter
                     , _dataQueue :: DataQueue
                     , _dataLighter :: Lighter
                     , _resultQueue :: ResultQueue
                     , _resultLighter :: Lighter
                     , _parser :: TmmParser
                     , _processor :: TmmProcessor
                     , _endFlag :: MVar Bool
                     }

newSpider :: TmmParser -> TmmProcessor -> IO Spider
newSpider prs prc = do
  taskq <- newChan
  dataq <- newChan
  resultq <- newChan
  tl <- newLighter
  dl <- newLighter
  rl <- newLighter
  ev <- newEmptyMVar
  return $ Spider taskq tl dataq dl resultq rl prs prc ev

isIdle :: Spider -> IO Bool
isIdle (Spider _ tl _ dl _ rl _ _ _) = do
  tb <- isLighterOn tl
  if tb
    then return False
    else do
      db <- isLighterOn dl
      if db
        then return False
        else isLighterOn rl

waitClosed :: Spider -> IO ()
waitClosed s =  takeMVar (_endFlag s) >> return ()


startSpider :: Spider -> StartsUrls -> IO ()
startSpider spider urls = do
  -- manager <- newManager tlsManagerSettings
  -- forkIO (downloading spider manager)
  downloader <- DL.newDownloader []
  forkIO (downloading spider downloader)
  forkIO (parsing spider)
  forkIO (processing spider >> putMVar (_endFlag spider) True)
  mapM_ (\(m, u) -> writeChan (_taskQueue spider) (RequestTask m u)) urls
  where
    downloading s@(Spider taskq tl dataq _ _ _ _ _ _) downloader = do
      withChannel2 "downloader" taskq (getVar tl) dataq $ \task -> do
        case task of
          RequestTask t url -> do
            (DL.TextData _ txt) <- DL.download downloader url
            return $ [ResponseText t txt]
    
    parsing s@(Spider _ _ dataq dl resultq _ p _ _) = do
      withChannel2 "parser" dataq (getVar dl) resultq $ \resp -> do
        case resp of
          ResponseText t txt -> return $ p t txt

    processing s@(Spider taskq _ _ _ resultq rl _ p _) =
      withChannel2 "processor" resultq (getVar rl) taskq $ \result -> do
       case result of
        DataResult t v -> p t v >> return []
        UrlResult t url -> return $ [RequestTask t url]
        _ -> error "should not come here"

readChannel :: Lighter -> Chan a -> IO a
readChannel lt chan = do
  turnOff lt
  r <- readChan chan
  turnOn lt
  return r

repeatRun :: String -> IO Bool -> IO ()
repeatRun msg p = startThread msg (repeatDo msg p) >> return ()

repeatRun2 :: String -> IO Bool -> IO () -> IO ()
repeatRun2 msg p last = startThread msg $ do
  repeatDo msg p
  last

startThread :: String -> IO () -> IO ()
startThread name p = do
  tid <- forkIO p
  putStrLn $ "Start thread " ++ name ++ ", id is " ++ show tid

repeatDo :: String -> IO Bool -> IO ()
repeatDo msg f = do
  b <- f
  if not b
    then putStrLn $ msg ++ " closed"
    else repeatDo msg f

waitEnd :: Spider -> IO ()
waitEnd spider = repeatDo "wait end"$ do
  b <- isIdle spider
  if b
    then return False
    else do
      threadDelay 50
      return True
  
askSpiderClose :: Spider -> IO ()
askSpiderClose s = do
  writeChan  (_taskQueue s) TaskEnd

runSpider :: StartsUrls -> TmmParser -> TmmProcessor -> IO ()
runSpider urls tp dp = do
  taskq  <- newChan
  dataq  <- newChan
  resultq <- newChan
  mapM_ (\(m, u) -> writeChan taskq (RequestTask m u)) urls
  startDownloader taskq dataq
  startParser dataq resultq tp
  startDataProcessor resultq taskq dp False

newLighter :: IO Lighter
newLighter = do
  mv <- newMVar False
  return $ Lighter mv

turnOn :: Lighter -> IO ()
turnOn t = switchLighter t True

turnOff :: Lighter -> IO ()
turnOff t = switchLighter t False

isLighterOn :: Lighter -> IO Bool
isLighterOn (Lighter mv) = do
  b <- takeMVar mv
  putMVar mv b
  return b
 
switchLighter :: Lighter -> Bool -> IO ()
switchLighter (Lighter mv) b = do
  _  <- takeMVar mv
  putMVar mv b
  
getVar :: Lighter -> (MVar Bool)
getVar (Lighter v) = v

startDownloader :: TaskQueue -> DataQueue -> IO ()
startDownloader taskq dataq = do
  manager <- newManager tlsManagerSettings
  startThread "downloader" (loop manager)
  where
    loop manager = do
      putStrLn "downloader: wait next task"
      task <- readChan taskq
      putStrLn "downloader: got task"
      case task of
        TaskEnd -> return ()
        RequestTask t url -> do
          txt <- fetchPage manager url
          writeChan dataq (ResponseText t txt)
          loop manager
  
fetchPage :: Manager -> String -> IO T.Text
fetchPage manager url = do
  putStrLn $ "* start download page " ++ url
  req <- parseRequest url
  resp <- httpLbs req manager
  gbk <- ICU.open "gbk" Nothing
  let txt :: T.Text
      txt = ICU.toUnicode gbk $ LB.toStrict $ responseBody resp
  putStrLn $ "* page downloaded "
  return txt


startParser :: DataQueue -> ResultQueue -> TmmParser -> IO ()
startParser dataq resultq p = do
  startThread "page parser" loop
  where
    loop = do
      putStrLn "parser: wait next page content"
      resp <- readChan dataq
      putStrLn "parser: got content"
      case resp of
        ResponseEnd -> return ()
        ResponseText t txt -> do
          let rx = p t txt
          mapM_ (writeChan resultq) rx
          loop
          
startDataProcessor :: ResultQueue -> TaskQueue -> TmmProcessor -> Bool -> IO ()
startDataProcessor resultq taskq p backend = 
  if backend
  then startThread "data processor" loop
  else loop
  where
    loop = do
      putStrLn "processor: wait result"
      result <- readChan resultq
      putStrLn "processor: got result"
      case result of
        ResultEnd -> return ()
        DataResult t v -> do
          p t v
          loop
        UrlResult t url -> do
          writeChan taskq $ RequestTask t url
          loop
  

data FlagChan a = FlagChan (Chan a) (MVar Bool)

withChannel :: String -> Chan a -> MVar Bool  -> (a -> IO b) -> IO b
withChannel msg ch v f =
  bracket
  (do
      putStrLn $ msg ++ " waiting next"
      readChan ch)
  (\_ -> modify v False)
  (\x -> do 
      modify v True
      putStrLn $ msg ++ " got one"
      y <- f x
      return y)
  where
    modify v b = do
      _ <- takeMVar v
      putMVar v b

withChannel2 :: (DataStream a, DataStream b) =>
                String -> Chan a -> MVar Bool  -> Chan b -> (a -> IO [b]) -> IO ()
withChannel2 msg cha v chb f =
  repeatDo msg $ bracket
  (do
      putStrLn $ msg ++ " waiting next"
      readChan cha)
  (\_ -> modify v False)
  (\x -> do 
      modify v True
      putStrLn $ msg ++ " got one"
      if isEnd x
        then do
          writeChan chb endGuard
          return False
        else do
          yx <- f x
          mapM_ (writeChan chb) yx
          return True)
  where
    modify v b = do
      _ <- takeMVar v
      putMVar v b
    
                     
    
