-- -*- coding: utf-8 -*-
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import TMM.Types
import TMM.Selector
import TMM.Workers
import TMM.Downloader

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Lazy.Char8 as B
import Text.HTML.TagSoup
import qualified Data.Text.ICU.Convert as ICU  -- text-icu
import qualified Data.Text.ICU as ICU
import qualified Data.Text.ICU.Regex as R
import qualified Data.List as L
import Text.StringLike
import Data.Maybe
import System.Environment (getArgs)
import Data.Aeson
import Control.Applicative ((<$>), (<*>))
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import Debug.Trace
  
main = do
  args <- getArgs
  let (ipg, npg) = parseArgs args
  
  spider <- newSpider mmParser mmProcessor
  startSpider spider (urls ipg npg)
  waitEnd spider 
  trace "ask spider close" $ askClose spider
  trace  "wait spider closed " $ waitClosed spider
  -- getLine
  where
    urls i n = map (((,) "list").gotPageUrl)  [i..(i + n -1)]
    parseArgs (a:b:_) = (read a::Int, read b::Int)
      
mmParser :: OriginData -> IO [YieldData]
mmParser od = case typeOf od  of
  "list" -> listPageParser od
  "detail" -> detailInfoParser od
  "photo" ->  photoPageParser od
  "image" -> return [bypass od]
  s -> error $ "unknown page type" ++ show s
  where
    bypass (OriginData t _ meta (OriginBinary bin)) = yieldBinary t meta bin


mmProcessor :: ResultData -> IO ()
mmProcessor (ResultData _ _ (ResultJson v)) = putStrLn $ show v
mmProcessor (ResultData _ meta (ResultBinary img)) = do
  let ids = T.unpack $ meta ! "uid"
  putStrLn "Got Image "
  B.writeFile ("./images/" ++ ids ++ ".jpg") img
            
detailInfoParser :: OriginData -> IO [YieldData]
detailInfoParser od = trace "detail page parser" $
  return $ css1 ".mm-p-base-info ul" pick tags
  where
    tags = parseTags $ originText od
    m2f fn = fn .= (metaOf od) ! fn
    pick tx = let height = fromMaybe "1" $ T.stripSuffix "CM" $ extract "ul .mm-p-height p" tx
                  weight = fromMaybe "1" $ T.stripSuffix "KG" $ extract "ul .mm-p-weight p" tx
                  size = extract "ul .mm-p-size p" tx
                  cup = extract "ul .mm-p-bar p" tx
                  shoe = extract "ul .mm-p-shose p" tx
              in  yieldJson "detail" M.empty $
                    object [ m2f "uid", m2f "name", m2f "age"
                           ,"height" .= height, "weight" .= weight, "size" .= size
                           , "cup" .= cup, "shoe" .= shoe
                           , m2f "photoUrl"]

photoPageParser :: OriginData -> IO [YieldData]
photoPageParser od = trace "photoPageParser run" $ do
  --putStrLn $ show $ originText od
  url <- fmap head $ searchImageUrl 1 (originText od) 
  return [yieldUrl "image"  (metaOf od) ("https:" ++ T.unpack url)]

searchImageUrl :: Int -> T.Text -> IO [T.Text]
searchImageUrl n ctx = do
  re <- R.regex [] "bigUrl&quot;:&quot;([^&]*)"
  R.setText re ctx
  T.putStrLn $ R.pattern re
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
    
listPageParser :: OriginData -> IO [YieldData]
listPageParser  od = trace "listPageParser run" $ return $ concat result
  where
    tags = parseTags $ originText od

    result :: [[YieldData]]
    result = css ".personal-info" pick tags
    
    pick1 :: [Tag T.Text] -> (T.Text, T.Text, T.Text)
    pick1 top = let name = extract ".top .lady-name" top
                    age = extract ".top em strong" top
                    uid:_ = css1  ".top .friend-follow"  ((fromAttrib "data-userid").head) top
                in (uid, name, age)
  
    pick :: [Tag T.Text] -> [YieldData]
    pick tx = let (uid, name, age):_ = css1 ".top" pick1 tx
                  url:_ =  css1 ".personal-info .w610 a" ((fromAttrib "href").head) tx
                  meta = M.fromList [ ("uid", uid), ("name", name), ("age", age), ("photoUrl", url)]
              in  [yieldUrl "detail" meta (gotDetailUrl uid),
                   yieldUrl "photo"  meta ("https:" ++ T.unpack url)]

     
gotPageUrl :: Int -> String
gotPageUrl i = "https://mm.taobao.com/json/request_top_list.htm?page=" ++ show i
-- gotPageUrl i = "http://localhost:8080/json/request_top_list.htm?page=" ++ show i

gotDetailUrl :: T.Text -> String
gotDetailUrl i =  "https://mm.taobao.com//self/info/model_info_show.htm?user_id= "
  ++ T.unpack i

detailUrl :: String
detailUrl = "https://mm.taobao.com//self/info/model_info_show.htm?user_id=189466234"

pageUrl :: String
pageUrl = "https://mm.taobao.com/json/request_top_list.htm?page=10"

data ModelBrief = ModelBrief { uid  :: T.Text
                             , name :: T.Text
                             , age  :: T.Text
                             , photoUrl :: T.Text
                             } deriving(Show)

instance FromJSON ModelBrief where
  parseJSON (Object v) = ModelBrief <$>
    (v .: "uid") <*>
    (v .: "name") <*>
    (v .: "age") <*>
    (v .: "photoUrl")

instance AsText ModelBrief where
  asText (ModelBrief a b c d) = T.concat ["ModelBrief " , a, "," , b, ", " , c, ", ", d]
 
data ModelInfo = ModelInfo T.Text T.Text T.Text T.Text 
                           T.Text T.Text T.Text T.Text T.Text deriving(Show)

instance AsText ModelInfo where
  asText (ModelInfo a b c d e f g h i) =
    T.concat ["ModelInfo " , a, "," , b, ", " , c, ", ", d,
              "," , e, ", " , f, ", ", g, ", " , h, ", ", i]

