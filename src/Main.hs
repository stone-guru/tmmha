-- -*- coding: utf-8 -*-
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import TMM.Types
import TMM.Selector
import TMM.Workers

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Lazy.Char8 as B
import Text.HTML.TagSoup
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.Text.ICU.Convert as ICU  -- text-icu
import qualified Data.Text.ICU as ICU
import qualified Data.List as L
import Text.StringLike
import Data.Maybe
import System.Environment (getArgs)
import Control.Concurrent.Chan
import Control.Concurrent (forkIO)
import Data.Aeson
import Control.Applicative ((<$>), (<*>))
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M

main = main3

main1 = do
--  bx <- B.readFile "/home/bison/Downloads/tpage10.html"
--  T.putStrLn txt
  args <- getArgs
  tags <- fetchContent (gotPageUrl $ parsePageNo args)
  let mx = parseListPage tags
  -- girls <- mapM fetchDetailInfo bx
  -- mapM_ (putStrLn . show) tags
  ch <- newChan
  let (ax, bx) = splitAt 5 mx
  forkIO (startDownload ch ax)
  forkIO (startDownload ch bx)
  models <- analys ch 2
  mapM_ (T.putStrLn . asText) models
  where
    parsePageNo [] = 10
    parsePageNo (s:_) = read s :: Int

main2 = do
  runSpider urls mmParser mmProcessor
  where
    urls = [(M.singleton "type" "list", gotPageUrl 112)]
             
main3 = do
  spider <- newSpider mmParser mmProcessor
  startSpider spider urls
  waitEnd spider
  askSpiderClose spider
  waitClosed spider
  -- getLine
  where
    urls = [(M.singleton "type" "list", gotPageUrl 112)]

mmParser :: Meta -> T.Text -> [ParseResult]
mmParser meta txt = case M.lookup "type" meta of
  Just "list" -> listPageParser meta txt
  Just "detail" -> detailInfoParser meta txt
  _ -> error $ "unknown page type" ++ show meta

mmProcessor :: Meta -> Value -> IO ()
mmProcessor meta v = putStrLn $ show v
  
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

detailInfoParser :: Meta -> T.Text -> [ParseResult]
detailInfoParser meta txt = pick $ select1 ".mm-p-base-info ul" tags
  where
    tags = parseTags txt
    m2f fn = fn .= (meta ! fn)
    pick tx = let height = fromJust $ T.stripSuffix "CM" $ extract "ul .mm-p-height p" tx
                  weight = fromJust $ T.stripSuffix "KG" $ extract "ul .mm-p-weight p" tx
                  size = extract "ul .mm-p-size p" tx
                  cup = extract "ul .mm-p-bar p" tx
                  shoe = extract "ul .mm-p-shose p" tx
              in  return $ DataResult (M.singleton "type" "detail") $
                    object [ m2f "uid", m2f "name", m2f "age"
                           ,"height" .= height, "weight" .= weight, "size" .= size
                           , "cup" .= cup, "shoe" .= shoe
                           , m2f "photoUrl"]

listPageParser :: Meta -> T.Text -> [ParseResult]
listPageParser  meta txt = map pick $ select ".list-item .personal-info" tags
  where
    pick1 tx = let top = select1 ".top" tx
                   name = extract ".top .lady-name" top
                   age = extract ".top em strong" top
                   uid = fromAttrib "data-userid" $ head $ select1 ".top .friend-follow" top
               in (uid, name, age)
    pick tx = let (uid, name, age) = pick1 tx
                  url = fromAttrib "href" $ head $ select1 ".personal-info .w610 a" tx
                  meta = M.fromList [("type", "detail"), ("uid", uid),
                                     ("name", name), ("age", age), ("photoUrl", url)]
              in UrlResult meta (gotDetailUrl uid)
    tags = parseTags txt

-- old pieces
parseDetailInfo :: ModelBrief -> [Tag T.Text] -> ModelInfo
parseDetailInfo (ModelBrief uid age name photoUrl) tags = pick $ select1 ".mm-p-base-info ul" tags
  where
    pick tx = let height = fromJust $ T.stripSuffix "CM" $ extract "ul .mm-p-height p" tx
                  weight = fromJust $ T.stripSuffix "KG" $ extract "ul .mm-p-weight p" tx
                  size = extract "ul .mm-p-size p" tx
                  cup = extract "ul .mm-p-bar p" tx
                  shoe = extract "ul .mm-p-shose p" tx
              in ModelInfo uid age name height weight size cup shoe photoUrl 


parseListPage :: [Tag T.Text] -> [ModelBrief]
parseListPage tags = map pick $ select ".list-item .personal-info" tags
  where
    pick1 tx = let top = select1 ".top" tx
                   name = extract ".top .lady-name" top
                   age = extract ".top em strong" top
                   uid = fromAttrib "data-userid" $ head $ select1 ".top .friend-follow" top
               in (uid, name, age)
    pick tx = let (uid, name, age) = pick1 tx
                  url = fromAttrib "href" $ head $ select1 ".personal-info .w610 a" tx
              in ModelBrief uid name age url

gotGirl :: [Tag T.Text] -> [T.Text]
gotGirl tags = map fetchOne $ sections (~== (str "<a class=lady-name>")) tags
  where
    fetchOne :: [Tag T.Text] -> T.Text
    fetchOne tags = let _:name:_ = tags
                        _:age:_ = dropWhile (~/= (str "<strong>")) tags
                        a:_ = dropWhile (~/= (str "<a>")) $
                                   dropWhile (~/= (str "<div class=\"pic w610\">")) tags
                        _:i:_ = T.split (== '=') $ fromAttrib "href" $ head tags
                    in L.foldl1' T.append [i, ",", (fromTagText name),
                                           ",", (fromTagText age), "," ,fromAttrib "href" a]

fetchContent :: String -> IO [Tag T.Text]
fetchContent url = do
  putStrLn $ "start download page " ++ url
  manager <- newManager tlsManagerSettings
  req <- parseRequest url
  resp <- httpLbs req manager
  gbk <- ICU.open "gbk" Nothing
  let txt :: T.Text
      txt = ICU.toUnicode gbk $ B.toStrict $ responseBody resp
  putStrLn $ "page download "

  return $ parseTags txt

str :: String -> String
str = id

text :: T.Text -> T.Text
text = id


fetchModelPage :: Manager -> ModelBrief -> IO (ModelBrief, T.Text)
fetchModelPage manager m@(ModelBrief i _ _ _) = do
  let url = gotDetailUrl i
  putStrLn $ "* start download page " ++ url
  req <- parseRequest url
  resp <- httpLbs req manager
  gbk <- ICU.open "gbk" Nothing
  let txt :: T.Text
      txt = ICU.toUnicode gbk $ B.toStrict $ responseBody resp
  putStrLn $ "* page downloaded "
  return (m, txt)

-- (str "<a class=lady-avatar>")

  
startDownload :: Chan (Maybe (ModelBrief, T.Text)) -> [ModelBrief] -> IO ()
startDownload chan xs = do
  manager <- newManager tlsManagerSettings
  loop manager xs
  where
    loop _ [] = writeChan chan Nothing 
    loop manager (m:mx) = do
      page <- fetchModelPage manager m
      writeChan chan (Just page)
      loop manager mx

analys :: Chan (Maybe (ModelBrief, T.Text)) -> Int -> IO [ModelInfo]
analys chan n = loop n []
  where
    loop 0 xs = return xs
    loop i xs = do
      n <- readChan chan
      case n of
        Nothing -> loop (i - 1) xs
        Just (m, txt) -> do
          let tags = parseTags txt
          let model = parseDetailInfo  m tags
          loop i (model:xs)
     
gotPageUrl :: Int -> String
gotPageUrl i = "https://mm.taobao.com/json/request_top_list.htm?page=" ++ show i

gotDetailUrl :: T.Text -> String
gotDetailUrl i =  "https://mm.taobao.com//self/info/model_info_show.htm?user_id= "
  ++ T.unpack i

detailUrl :: String
detailUrl = "https://mm.taobao.com//self/info/model_info_show.htm?user_id=189466234"

pageUrl :: String
pageUrl = "https://mm.taobao.com/json/request_top_list.htm?page=10"
