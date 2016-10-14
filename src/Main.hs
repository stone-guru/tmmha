-- -*- coding: utf-8 -*-
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import TMM.Types
import TMM.Selector
import qualified TMM.Workers as S
import TMM.Downloader

import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
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
import Database.PostgreSQL.Simple as DB
import Control.Monad

data AppContext = AppContext { _dbConnection :: DB.Connection
                             , _stageNum :: Int
                             }

main = do
  args <- getArgs
  let (ipg, npg) = parseArgs args
  config <- initConfig
  sum <- S.executeAction config (urls ipg npg)
  verbose sum
  where
    urls i n = map (\i -> (pageUrl i,  "list")) [i..(i + n -1)]
    initConfig = do
      ctx <- initAppContext
      return S.defaultConfig { S._cParsers = parsers
                             , S._cProcessors = processors ctx}
    parseArgs (a:b:_) = (read a::Int, read b::Int)
    parsers = [ ("list", listPageParser, Tags)
              , ("detail", detailPageParser, Tags)
              , ("photo" , photoPageParser, UtfText)
              , ("image" , S.transmitParser, RawBinary)]
    processors ctx = [ ("detail", detailInfoProcessor ctx) --valueProcessor
                     , ("image", imageProcessor)]
    toM :: Int -> Double
    toM x = fromIntegral x / (1024 * 1024)

    verbose sum = putStrLn $ printf "download: resources %d, total %.3f MB"
                     (S._sDownloadCount sum) (toM $ S._sDownloadBytes sum)

initAppContext :: IO AppContext
initAppContext = do
  conn <- connect defaultConnectInfo { connectHost = "localhost"
                                     , connectDatabase = "tmm"
                                     , connectUser = "bison"
                                     , connectPassword = "123456"
                                     }
  [Only stage] <- withTransaction conn
    (query_ conn "select f_stage_num()" :: IO [Only Int])
  return $ AppContext conn stage

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

t2i :: Text -> Int
t2i txt = case T.decimal txt of
            Right (i, _) -> i
            Left msg -> 0

t2f :: Text -> Float
t2f txt = case T.double txt of
            Right (x, _) -> realToFrac x
            Left msg -> 0.0


detailPageParser :: SourceData -> IO [YieldData]
detailPageParser od = trace "detailPageParser run" $ 
  return [runSelector (originTags od) selector]
  where
    selector = one ".mm-p-base-info ul" $ do
      birthDate <- birthp <$> searchText "li:nth-child(2)" "([0-9]+)月 *([0-9]+)日"
      height <- (t2f . fst . T.breakOn "CM") <$> textOf ".mm-p-height p"
      weight <- (t2f . fst . T.breakOn "KG") <$> textOf ".mm-p-weight p"
      [waist, bust, hip] <- parseSize <$> textOf ".mm-p-size p"
      cup <- takeCup <$> textOf ".mm-p-bar p"
      shoe <- textOf "ul .mm-p-shose p"
      return $ yieldJson od $ object [ "uid" .= t2i (meta ! "uid")
                                     , "name" .= meta ! "name", "birthDate" .= birthDate
                                     , "waist" .= waist, "bust" .= bust, "hip" .= hip
                                     , "weight" .= weight, "height" .= height, "cup" .= cup
                                     ]
    meta = metaOf od
    parseSize s = map t2f (T.splitOn "-" s)
    birthp [] = birthp ["01", "01"]
    birthp ["", ""] = birthp ["01", "01"]
    birthp [ms, ds] = T.concat [(T.pack $ show (2016 - t2i (meta ! "age")))
                              , "-", ms, "-", ds]
    takeCup s = fromJust $ L.find (`T.isSuffixOf` s ) ["C", "E", "D", "B", "F", "A", ""]

detailInfoProcessor :: AppContext -> ResultData -> IO ()
detailInfoProcessor ctx (ResultData _ (RJson v)) = do
  print v
  void $ withTransaction (_dbConnection ctx) $ 
        case parse (paramParser $ _stageNum ctx)  v of
          Error s -> error s
          Success params -> query (_dbConnection ctx)
                              "select 1 from insert_model(?,?,?,?,?,?,?,?,?,?)"  params :: IO [Only Int]
  where
    paramParser st = withObject "modelinfo" $ \o -> do
      uid :: Int <- o .: "uid"
      name :: Text <- o .: "name"
      birthDate :: Text <- o .: "birthDate"
      height :: Float <- o .: "height"
      weight :: Float <- o .: "weight"
      waist :: Float <- o .: "waist"
      bust :: Float <- o .: "bust"
      hip :: Float <- o .: "hip"
      cup :: Text <- o .: "cup"
      return (st, uid, name, birthDate, height, weight, waist, bust, hip, cup)

valueProcessor :: ResultData -> IO ()
valueProcessor rd = print (resultValue rd)

photoPageParser :: SourceData -> IO [YieldData]
photoPageParser src = trace "photoPageParser run" $ do
  urls <- searchImageUrl nImage (originText src)
  return $ flip map (zip [1..] urls) $ \(i, url) ->
    yieldUrl src (T.append "https:" url) (M.insert "imageIndex" (asText i) $ metaOf src) "image"
  where
    asText = T.pack.show
    nImage = 2

imageProcessor :: ResultData -> IO ()
imageProcessor (ResultData td (RBinary bytes)) = do
  let meta = _tdMeta td
  let index = M.lookupDefault "1" "imageIndex" meta 
  let fn =  FP.fromText $ T.concat ["./images/" , meta ! "uid", "-",  meta ! "name", "-", index, ".jpg"]
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
