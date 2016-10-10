-- -*- coding: utf-8 -*-
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import TMM.Types
import TMM.Selector
import qualified TMM.Workers as S
import TMM.Downloader

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
import Control.Applicative ((<$>), (<*>))
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import Debug.Trace
import qualified Text.HTML.TagSoup.Fast as F
import System.IO
import qualified Filesystem.Path.CurrentOS  as FP

main = do
  args <- getArgs
  let (ipg, npg) = parseArgs args
  sum <- S.executeAction config (urls ipg npg)
  verbose sum
  where
    config = S.defaultConfig { S._cParsers = parsers
                             , S._cProcessors = processors}
    urls i n = map (\i -> (pageUrl i,  "list")) [i..(i + n -1)]
    parseArgs (a:b:_) = (read a::Int, read b::Int)
    parsers = [ ("list", listPageParser, Tags)
              , ("detail", detailPageParser, Tags)
              , ("photo" , photoPageParser, UtfText)
              , ("image" , S.transmitParser, RawBinary)]
    processors = [ ("detail", valueProcessor)
                 , ("image", imageProcessor)]
    toM :: Int -> Double
    toM x = fromIntegral x / (1024 * 1024)

    verbose sum = do
      putStrLn $ printf "download: resources %d, total %.3f MB"
                        (S._sDownloadCount sum) (toM $ S._sDownloadBytes sum)

pageUrl :: Int -> Text
pageUrl i = let base = "https://mm.taobao.com/json/request_top_list.htm?page="
            in T.append base $ T.pack (show i)

detailPageUrl :: Text -> Text
detailPageUrl =  T.append "https://mm.taobao.com/self/info/model_info_show.htm?user_id="

listPageParser :: SourceData -> IO [YieldData]
listPageParser  od  = trace "listPageParser run" $ do
  let result = L.concat $ runSelector (originTags od) selector
  -- mapM_ print result
  return result
  where
    selector = many ".personal-info" $ do
      name <- textOf ".top .lady-name"
      uid <-  attrOf ".top .friend-follow" "data-userid"
      age <-  textOf ".top em strong"
      url <- attrOf  ".w610 a" "href"
      let meta = M.fromList [ ("uid", uid), ("name", name), ("age", age)]
      return [ yieldUrl  od (detailPageUrl uid) meta "detail"
             , yieldUrl od (T.append "https:" url) meta "photo"]

detailPageParser :: SourceData -> IO [YieldData]
detailPageParser od = trace "detailPageParser run" $ do
  return [runSelector (originTags od) selector]
  where
    m2f fn = fn .= (metaOf od) ! fn
    selector = one ".mm-p-base-info ul" $ do
      height <- fmap (T.stripSuffix "CM") (textOf ".mm-p-height p")
      weight <- fmap (T.stripSuffix "KG") (textOf ".mm-p-weight p")
      size <- textOf ".mm-p-size p"
      cup <- textOf ".mm-p-bar p"
      shoe <- textOf "ul .mm-p-shose p"
      return $ yieldJson od $ object [ m2f "uid" , m2f "name", m2f "age"
                                     , "size" .= size, "weight" .= weight
                                     , "height" .= height
                                     , "cup" .= cup, "shoe" .= shoe
                                     ]

valueProcessor :: ResultData -> IO ()
valueProcessor rd = putStrLn $ show $ resultValue rd
  
photoPageParser :: SourceData -> IO [YieldData]
photoPageParser src = trace "photoPageParser run" $ do
  urls <- searchImageUrl nImage (originText src)
  return $ flip map urls $ \url ->
    yieldUrl src (T.append "https:" url) (metaOf src) "image"
  where
    nImage = 2

imageProcessor :: ResultData -> IO ()
imageProcessor (ResultData td (RBinary bytes)) = do
  let meta = _tdMeta td
  let fn =  FP.fromText $ T.concat ["./images/" , meta ! "uid", "-",  meta ! "name" , ".jpg"]
  T.putStrLn $ T.append "save image file "  (either id id $ FP.toText fn)
  B8.writeFile (FP.encodeString fn) bytes

searchImageUrl :: Int -> T.Text -> IO [T.Text]
searchImageUrl n ctx = do
  re <- R.regex [] "bigUrl&quot;:&quot;([^&]*)"
  R.setText re ctx
  -- T.putStrLn $ R.pattern re
  b <- R.find re 0
  loop b n ctx re []
  where
    loop False _ _ _ sx = return sx
    loop _ 0 _ _ sx = return sx
    loop True i s re sx = do
      start <- R.start_ re 1
      end <-   R.end_ re 1
      --putStrLn $ "found one" ++ show start ++ ", " ++ show end
      if start > -1
        then  do
          let ss = sub s (fromEnum start) (fromEnum end)
          --T.putStrLn $ ss
          b <- R.findNext re
          loop b (i - 1) s re (ss:sx)
        else do
          b <- R.findNext re
          loop b (i - 1) s re sx
    sub s start end = let s1 = T.take end s
                          s2 = T.drop start s1
                      in s1 `seq` s2
