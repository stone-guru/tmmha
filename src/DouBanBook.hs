-- -*- coding: utf-8 -*-
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

import TMM.Types
import TMM.Selector
import qualified TMM.Workers as S
import TMM.Downloader

import qualified Data.Char as C
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Char8 as B8
import Text.HTML.TagSoup
import qualified Data.Text.ICU.Convert as ICU  -- text-icu
import qualified Data.Text.ICU as ICU
import qualified Data.Text.ICU.Regex as R
import qualified Data.List as L
import Text.Printf
import Data.Maybe
import System.Environment (getArgs)
import Data.Aeson
import Data.Aeson.Types
import Control.Applicative ((<$>), (<*>))
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import Debug.Trace
import qualified Text.HTML.TagSoup.Fast as F
import System.IO
import qualified Filesystem.Path.CurrentOS  as FP
import Control.Monad
import Data.Typeable
import Control.DeepSeq

main = do
  args <- getArgs
  let (bookTag, iStart, iEnd) = parseArgs args
  hd <- openFile "./books.csv" AppendMode
  let (url, meta)  = listPageUrl bookTag iStart
  config <- initConfig iEnd hd
  -- sum <- S.executeAction config startUrls -- [(url, meta, "list")]
  sum <- S.executeAction config [(url, meta, "list")]
  hClose hd
  verbose sum
  where
    initConfig iEnd hd = do
      return S.defaultConfig { S._cParsers = parsers iEnd
                             , S._cProcessors = processors hd}
    startUrls = [(book6958, M.empty, "book")]
    parseArgs (s1:s2:s3:_) = (T.pack s1, read s2::Int, read s3::Int)
    parsers iEnd = [ ("list", listPageParser iEnd, Tags)
                   , ("book", bookPageParser, Tags)]
    processors hd = [("book", bookInfoProcessor hd)] --valueProcessor
    toM :: Int -> Double
    toM x = fromIntegral x / (1024 * 1024)

    verbose sum = putStrLn $ printf "download: resources %d, total %.3f MB"
                     (S._sDownloadCount sum) (toM $ S._sDownloadBytes sum)

book6958 = "file:///home/bison/sources/haskell/tmmha/test2/data/book26596958.html"

listPageUrl :: Text -> Int -> (Text, Meta)
listPageUrl bookTag iStart = (T.concat ["https://www.douban.com/tag/", bookTag, "/book?start=", sStart],
                              M.fromList [("bookTag", bookTag), ("iStart", sStart)])
  where sStart = T.pack (show iStart)

listPageParser :: Int -> SourceData -> IO [YieldData]
listPageParser maxItem od  = trace "listPageParser run" $ do
  let result =  runSelector (originTags od) selector
  -- mapM_ print result
  return result
  where
    selector = do
      books <- many "dl dd" $ do
        url <-  attrOf ".title" "href"
        name <-  textOf ".title"
        return $ yieldUrl  od url (metaOf od) "book"
      if (null books)
        then return []
        else let iStart = 15 + t2i (metaOf od ! "iStart")
                 bookTag = metaOf od ! "bookTag"
                 (listUrl, meta) = listPageUrl bookTag iStart
             in if iStart > maxItem
                then return books
                else return $ (yieldUrl od listUrl meta "list"):books
  
data Book = Book !Text !Text !Text !Text !Text !Text deriving(Show, Typeable)

instance NFData Book where
  rnf (Book s1 s2 s3 s4 s5 s6) = rnf ()

bookPageParser :: SourceData -> IO [YieldData]
bookPageParser od = do
  -- putStrLn $ show (originTags od)
  trace "bookPageParser run" $
    return [runSelector (originTags od) selector]
  where
    selector = do
      bname <- at "#wrapper" $ textOf "h1 span"
      bf <- at "#info" $ Book bname  
            <$> (trim <$> textOf "span:nth-child(1) a")
            <*> (trim <$> textAfter "span[text|='出版社']")
            <*> (trim <$> textAfter "span[text|='页数']")
            <*> (trim <$> textAfter "span[text|='定价']")
      book <- bf <$> (trim <$> textOf ".rating_num")
      return $ yieldData od book

bookInfoProcessor :: Handle -> ResultData -> IO ()
bookInfoProcessor hd rd = do
  let (Book name author publisher totalPage price rating) = resultData rd
      s = T.intercalate "\", \"" [name, author, publisher, totalPage, price, rating]
      line = T.concat ["\"", s, "\""]
  T.hPutStrLn hd line >> hFlush hd

-- bookInfoProcessor :: Handle -> ResultData -> IO ()
-- bookInfoProcessor hd (ResultData _ (RJson v)) = do
--   print v
--   void $ case parse parser v of
--            Error s -> error s
--            Success line -> T.hPutStrLn hd line >> hFlush hd
--   where
--     parser = withObject "modelinfo" $ \o -> do
--       name :: Text <- o .: "name"
--       author :: Text <- o .: "author"
--       publisher :: Text <- o .: "publisher"
--       totalPage :: Text <- o .: "totalPage"
--       price :: Text <- o .: "price"
--       rating :: Text <- o .: "rating"
--       return $ let s = T.intercalate "\", \"" [name, author, publisher, totalPage, price, rating]
--                in T.concat ["\"", s, "\""]
