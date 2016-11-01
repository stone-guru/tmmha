-- -*- coding: utf-8 -*-
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import TMM.Types
import TMM.Selector
import qualified TMM.Workers as S
import TMM.Downloader
import ZhiHu.ZSession

import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.HTML.TagSoup
import qualified Data.List as L
import Data.Maybe
import System.Environment (getArgs)
import Data.Aeson
import Data.Aeson.Types
import Control.Applicative ((<$>), (<*>))
import Debug.Trace
import System.IO
import Control.Monad
import qualified Data.HashMap.Strict as M
import Data.Typeable
import Control.DeepSeq

main = do
  config <- initConfig
  sum <- S.executeAction config startUrls
  S.verbose sum
  where
    initConfig = do
      return S.defaultConfig { S._cParsers = [("topic", topicPageParser, Tags)]
                             , S._cProcessors = [ ("topic", valuePrinter)]
                             , S._cInitSession = Just $ zhiHuLogin
                             , S._cCookieAction = Just $ S.CookieAction updateCookieXsrf
                             , S._cEnableCookie = True
                             , S._cFixAgent = True
                             }
    startUrls = [("https://www.zhihu.com/topic", M.empty, "topic")]

data Topic = Topic Text Text Text Int deriving (Show, Typeable)
instance NFData Topic where
  rnf (Topic topic url vote author) = rnf topic `seq` rnf url `seq` rnf vote `seq` rnf author
  
topicPageParser :: SourceData -> IO [YieldData]
topicPageParser od = trace "topicPageParser run" $
  return $ runSelector (originTags od) selector
  where
    selector = at "#zh-topic-feed-list" $ many ".feed-item .feed-content" $ do
      topic <- textOf "h2"
      url <- attrOf ".entry-body link" "href"
      nVote <- t2i <$> textOf ".zm-item-vote a"
      author <- textOf ".author-link-line a"
      return $ yieldData od $ Topic topic url author nVote

valuePrinter :: ResultData -> IO ()
valuePrinter rd = do
  let (Topic topic url author vote) = resultData rd
  T.putStrLn "--------------------------------------------------"
  T.putStrLn $ T.concat ["topic: ", topic, "\nurl: ", url,
                         "\nvote: ", T.pack $ show vote, "\nauthor: ", author]

