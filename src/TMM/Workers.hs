-- -*- coding: utf-8 -*-
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TMM.Workers(
  Spider(),
  newSpider,
  startSpider,
  isIdle,
  waitIdle,
  waitEnd,
  askClose,
  waitClosed
  )where

import TMM.Types
import qualified TMM.Downloader as DL
import Control.Monad  
import Control.Concurrent
import Control.Concurrent.Chan
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.List as L
import Control.Exception
import qualified Data.HashMap.Strict as M
import Control.DeepSeq

class Octopus a where
  newInstance :: ShParser -> ShProcessor -> IO a
  isInstIdle :: a -> IO Bool
  waitIntClosed :: a -> IO ()

type PilotLight = MVar Bool

data Spider = Spider { _taskQueue :: TaskQueue
                     , _taskLight :: PilotLight
                     , _dataQueue :: DataQueue
                     , _dataLight :: PilotLight
                     , _resultQueue :: ResultQueue
                     , _resultLight :: PilotLight
                     , _parser :: ShParser
                     , _processor :: ShProcessor
                     , _endFlag :: MVar Bool
                     }

newSpider :: ShParser -> ShProcessor -> IO Spider
newSpider prs prc = do
  taskq <- newChan
  dataq <- newChan
  resultq <- newChan
  tpl <- newMVar False
  dpl <- newMVar False
  rpl <- newMVar False
  ef <- newEmptyMVar
  return $ Spider taskq tpl dataq dpl resultq rpl prs prc ef

startSpider :: Spider -> StartsUrls -> IO ()
startSpider spider urls = do
  fdownload <- DL.newDownloader []
  forkIO (downloadRoute spider fdownload "downloader1")
  -- forkIO (downloadRoute spider fdownload "downloader2")
  forkIO (parseRoute spider)
  forkIO (processRoute spider >> putMVar (_endFlag spider) True)
  mapM_  feedStartsUrl urls
  where
    feedStartsUrl (t, url) = writeChan (_taskQueue spider) $ Just $ TaskData t M.empty url

downloadRoute :: Spider -> DL.Downloader -> String -> IO ()
downloadRoute s dl rn = mapChannel rn
                         (_taskQueue s) (_taskLight s) (_dataQueue s) download
  where
    download (TaskData t meta url) = do
      d <- dl url
      case d of
        DL.ResponseData url ctype ctx ->
          return $! [OriginData t  ctype (fillUrl url meta) (ctx2Origin ctx)]
        DL.NoData -> do
          putStrLn $ "error got page " ++ url
          return []
    fillUrl url meta = M.insert "url" (T.pack url) meta
    ctx2Origin (Left text) = OriginText text
    ctx2Origin (Right bin) = OriginBinary bin


parseRoute :: Spider -> IO ()
parseRoute s = mapChannel "parser"
                 (_dataQueue s) (_dataLight s) (_resultQueue s)
                 (\x -> do
                     y <- _parser s x
                     return $! y)

processRoute :: Spider -> IO ()
processRoute s = mapChannel "process"
                    (_resultQueue s) (_resultLight s) (_taskQueue s) process
  where
    process yd = if isUrl yd then return [toTask yd] else
      do
        _processor s (yd2rd yd)
        return []
  
    isUrl (YieldData _ _ (YieldUrl _)) = True
    isUrl _ = False
  
    toTask (YieldData t meta (YieldUrl url)) = TaskData t meta url
  
    yd2rd (YieldData t m ent) = ResultData t m (ye2re ent)
    ye2re (YieldBinary b) = ResultBinary b
    ye2re (YieldText t) = ResultText t
    ye2re (YieldJson j) = ResultJson j
    ye2re _ = error "unhandled YieldData Type"

isIdle :: Spider -> IO Bool
isIdle (Spider _ tl _ dl _ rl _ _ _) = do
  tb <- isTurnedOn "downloader" tl
  if tb
    then return False
    else do
      db <- isTurnedOn "parser" dl
      if db
        then return False
        else isTurnedOn "processor" rl >>= return.not

waitIdle :: Spider -> Int -> IO ()
waitIdle s ms = isIdle s >>= \b -> 
  if b then return () else do
    threadDelay ms
    waitIdle s ms

waitEnd :: Spider -> IO ()
waitEnd spider = repeatDo "wait end"$ do
  threadDelay $ 1000 * 1000
  b <- isIdle spider
  return $ not b

askClose :: Spider -> IO ()
askClose s = do
  writeChan  (_taskQueue s) Nothing
 
waitClosed :: Spider -> IO ()
waitClosed s =  void $ takeMVar (_endFlag s)

mapChannel :: String -> Chan (Maybe a) -> PilotLight  -> Chan (Maybe b) -> (a -> IO [b]) -> IO ()
mapChannel msg cha pla chb f =
  repeatDo msg $ bracket
  (do
      putStrLn $ msg ++ " waiting next"
      readChan cha)
  (\_ -> do
      --putStrLn $ msg ++ "turn Off"
      turnOff pla)
  (\x_ -> do
      turnOn pla
      putStrLn $ msg ++ " got one"
      --putStrLn $ msg ++ " turn On"
      case x_ of
        Nothing -> do
          writeChan chb Nothing
          return False
        Just x -> do
          yx_ <- f x
          yx <- evaluate yx_
          mapM_ (writeChan chb . Just)  yx
          return True)


turnOn :: PilotLight -> IO ()
turnOn t = switch t True


turnOff :: PilotLight -> IO ()
turnOff t = switch t False

isTurnedOn :: String -> PilotLight -> IO Bool
isTurnedOn msg mv = do
  b <- takeMVar mv
  putMVar mv b
  -- putStrLn $ "check " ++ msg ++ " lighter is" ++ show b
  return b

switch :: PilotLight -> Bool -> IO ()
switch mv b = takeMVar mv >> putMVar mv b

repeatDo :: String -> IO Bool -> IO ()
repeatDo msg f = do
  b <- f
  if not b
    then putStrLn $ msg ++ " closed"
    else repeatDo msg f
