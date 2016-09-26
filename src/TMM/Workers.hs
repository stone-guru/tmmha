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
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.MVar
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.List as L
import Control.Exception
import qualified Data.HashMap.Strict as M
import Control.DeepSeq
import qualified Data.Text.Encoding as E
import qualified Data.Text.ICU.Convert as ICU  -- text-icu
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.ByteString as B
import Network.HTTP.Client
import Network.HTTP.Types.Header

class Octopus a where
  newInstance :: ShParser -> ShProcessor -> IO a
  isInstIdle :: a -> IO Bool
  waitIntClosed :: a -> IO ()

-- 
type TaskQueue = TChan (Maybe TaskData)
type DataQueue = TChan (Maybe OriginData)
type ResultQueue = TChan (Maybe YieldData)


type PilotLight = TVar Bool

data Spider = Spider { _taskQueue :: TaskQueue
                     , _taskLight :: PilotLight
                     , _dataQueue :: DataQueue
                     , _dataLight :: PilotLight
                     , _resultQueue :: ResultQueue
                     , _resultLight :: PilotLight
                     , _parser :: ShParser
                     , _processor :: ShProcessor
                     , _workingFlag :: MVar Bool
                     }

newSpider :: ShParser -> ShProcessor -> IO Spider
newSpider prs prc = do
  ef <- newEmptyMVar
  atomically $ do
    taskq <- newTChan
    dataq <- newTChan
    resultq <- newTChan
    tpl <- newTVar False
    dpl <- newTVar False
    rpl <- newTVar False
    return $ Spider taskq tpl dataq dpl resultq rpl prs prc ef

startSpider :: Spider -> StartsUrls -> IO ()
startSpider spider urls = do
  fdownload <- DL.newDownloader []
  forkIO (downloadRoute spider fdownload "downloader1")
  -- forkIO (downloadRoute spider fdownload "downloader2")
  forkIO (parseRoute spider)
  forkIO (processRoute spider >> putMVar (_workingFlag spider) False)
  atomically $ mapM_  feedStartsUrl urls
  putMVar (_workingFlag spider) True
  where
    feedStartsUrl (t, url) = writeTChan (_taskQueue spider) $
                                        Just $ TaskData t M.empty url

downloadRoute :: Spider -> DL.Downloader -> String -> IO ()
downloadRoute s dl rn = mapChannel rn
                         (_taskQueue s) (_taskLight s) (_dataQueue s) download
  where
    download (TaskData t meta url) = do
      response <- dl url
      let headers = responseHeaders  response
      let bytes = responseBody response
      
      case parseContentType $ lookup hContentType headers of
        (False, Just ctype, _) -> do
          return [OriginData t  ctype (fillUrl url meta) (OriginBinary bytes)]
      
        (True, Just ctype, charset_) -> do
          let charset = maybe "utf-8" id charset_
          txt <- unicoding charset bytes
          return $ [OriginData t  ctype (fillUrl url meta) (OriginText txt)]

    fillUrl url meta = M.insert "url" (T.pack url) meta
    ctx2Origin (Left text) = OriginText text
    ctx2Origin (Right bin) = OriginBinary bin

b2s :: B.ByteString -> String
b2s = map (toEnum . fromIntegral) . B.unpack

parseContentType :: Maybe B.ByteString -> (Bool, Maybe T.Text, Maybe String)
parseContentType Nothing =
  (False, Just "application/octet-stream", Nothing)
parseContentType (Just ct) =
  let isText = (B.isPrefixOf "text/" ct) || (B.isPrefixOf "application/js" ct)
      contentType = fst $ B.breakSubstring ";" ct
      charset = let rest = snd (B.breakSubstring "=" ct)
                in if B.null rest then Nothing else Just $ b2s (B.tail rest)
  in (isText, Just (E.decodeUtf8 contentType), charset)

unicoding :: String -> B8.ByteString -> IO T.Text
unicoding charset bytes = do
  locale <- ICU.open charset Nothing
  let txt = ICU.toUnicode locale $ B8.toStrict bytes
  return txt

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
waitEnd spider = do
  readMVar (_workingFlag spider) --assure spider is working
  repeatDo "wait end"$ do
    threadDelay $ 1000 * 100
    b <- isIdle spider
    return $ not b

askClose :: Spider -> IO ()
askClose s = atomically $ writeTChan  (_taskQueue s) Nothing
 
waitClosed :: Spider -> IO ()
waitClosed s =  void $ takeMVar (_workingFlag s)

mapChannel :: (NFData a, NFData b) =>  String -> TChan (Maybe a) -> PilotLight  ->
              TChan (Maybe b) -> (a -> IO [b]) -> IO ()
mapChannel msg cha pla chb f =
  repeatDo msg $ bracket
  (do
      putStrLn $ msg ++ " waiting next"
      atomically $ readTChan cha)
  (\_ -> do
      --putStrLn $ msg ++ "turn Off"
      turnOff pla)
  (\x_ -> do
      turnOn pla
      putStrLn $ msg ++ " got one"
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
