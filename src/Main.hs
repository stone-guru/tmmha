-- -*- coding: utf-8 -*-
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import TMM.Types
import TMM.Selector
import qualified TMM.Workers as S
import TMM.Downloader

import qualified Data.Char as C
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

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Network.HTTP.Client
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LB8

import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString

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
    initConfig = do
      ctx <- initAppContext
      return S.defaultConfig {
        S._cParsers = [ ("list", listPageParser, Tags)
                      , ("detail", detailPageParser, Tags)
                      , ("photo" , photoPageParser, RawBinary)
                      , ("image" , S.transmitParser, RawBinary)],
        S._cProcessors = [ ("detail", detailInfoProcessor ctx)
                         , ("image", imageProcessor)],
        S._cDownloadInterval = 0
        }
        
    parseArgs (a:b:_) = (read a::Int, read b::Int)
  
    urls i n = map (\i -> (pageUrl i, M.empty, "list")) [i..(i + n -1)]

    toM :: Int -> Double
    toM x = fromIntegral x / (1024 * 1024)

    verbose sum = do
      putStrLn $ printf "Download: resources %d, total %.3f MB"
        (S._sDownloadCount sum) (toM $ S._sDownloadBytes sum)
      putStrLn $ "Parser statistics"
      mapM (putStrLn . ("  " ++) . show) (S._sParExecStat sum)
      putStrLn $ "Processor statistics"
      mapM (putStrLn . ("  " ++) . show) (S._sPrcExecStat sum)


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

detailPageParser :: SourceData -> IO [YieldData]
detailPageParser od = trace "detailPageParser run" $ 
  return [runSelector (originTags od) selector]
  where
    selector = at ".mm-p-base-info ul" $ do
      birthDate <- birthp <$> searchText "li:nth-child(2)" "([0-9]+)月 *([0-9]+)日"
      height <- (t2f . fst . T.breakOn "CM") <$> textOf ".mm-p-height p"
      weight <- (t2f . fst . T.breakOn "KG") <$> textOf ".mm-p-weight p"
      [waist, bust, hip] <- parseSize <$> textOf ".mm-p-size p"
      -- cup <- takeCup <$> textOf ".mm-p-bar p"
      cup <- takeCup <$> textOf "li:nth-last-child(2) p"
      return $ yieldJson od $ object [ "uid" .= t2i (meta ! "uid")
                                     , "name" .= meta ! "name", "birthDate" .= birthDate
                                     , "waist" .= waist, "bust" .= bust, "hip" .= hip
                                     , "weight" .= weight, "height" .= height, "cup" .= cup
                                     ]
    meta = metaOf od
    parseSize s = map t2f (T.splitOn "-" s)
    birthp [] = birthp ["", ""]
    birthp ["", ""] = birthp ["01", "01"]
    birthp [ms, ds] = T.concat [(T.pack $ show (2016 - t2i (meta ! "age")))
                              , "-", ms, "-", ds]
    takeCup s = if T.null s then ""
                else let c = C.toUpper $ T.last s
                     in if C.isLetter c then T.singleton c else ""

detailInfoProcessor :: AppContext -> ResultData -> IO ()
detailInfoProcessor ctx (ResultData _ (RJson v)) = do
  print v
  void $ withTransaction (_dbConnection ctx) $ 
        case parse (paramParser $ _stageNum ctx)  v of
          Error s -> error s
          Success params -> query (_dbConnection ctx)
                              "select 1 from insert_model(?,?,?,?,?,?,?,?,?,?)"
                              params :: IO [Only Int]
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
  --urls <- searchImageUrl nImage (originText src)
  let nImage = 2
  let urls = take nImage $ searchUrl (originBytes src)
  return $ flip map (zip [1..] urls) $ \(i, url) ->
    yieldUrl src url (M.insert "imageIndex" (asText i) $ metaOf src) "image"
  where
    searchUrl :: B8.ByteString -> [Text]
    searchUrl bytes = L.unfoldr (\s -> match $ s =~  ptn) bytes 
    match :: (B8.ByteString, B8.ByteString, B8.ByteString, [B8.ByteString])
          -> Maybe (Text, B8.ByteString)
    match (_, _, _, []) = Nothing
    match (_, _, rest, [!url]) = Just (T.append "https:" (E.decodeUtf8 url), rest)
    ptn = "bigUrl&quot;:&quot;([^&]*)" :: B8.ByteString  
    asText = T.pack . show

imageProcessor :: ResultData -> IO ()
imageProcessor (ResultData td (RBinary bytes)) = do
  T.putStrLn $ T.append "save image file "  (either id id $ FP.toText fn)
  B8.writeFile (FP.encodeString fn) bytes
  where
    meta = _tdMeta td
    index = M.lookupDefault "1" "imageIndex" meta
    suffix = imageFormat bytes
    fn =  FP.fromText $ T.concat ["./images/"
                                   , meta ! "uid", "-",  meta ! "name", "-", index, suffix]

imageFormat :: B8.ByteString -> Text
imageFormat bytes
      |"\o377\o330" `B8.isPrefixOf` bytes = ".jpg"
      |"\o211PNG\r\n\o032\n"  `B8.isPrefixOf` bytes = ".png"
      |"GIF87a" `B8.isPrefixOf` bytes = ".gif"
      |"GIF89a" `B8.isPrefixOf` bytes = ".gif"
      |"BM" `B8.isPrefixOf` bytes = ".bmp"
      |otherwise = ".jpg"

