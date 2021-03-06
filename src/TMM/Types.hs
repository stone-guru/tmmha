-- -*- coding: utf-8 -*-
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module TMM.Types where

import Control.Concurrent.Chan (Chan)
import Data.Text (Text)
import Data.Aeson (Value)
import Data.HashMap.Strict (HashMap, empty)
import Data.ByteString.Char8 (ByteString)
import Control.DeepSeq
import Text.HTML.TagSoup (Tag)
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Data.Typeable
import Data.Time.Clock

type TextTag = Tag Text
type Meta = HashMap Text Text

data TaskDesc = TaskDesc { _tdUrl :: !Text
                         , _tdMeta :: !Meta
                         , _tdHandler :: !Text
                         }deriving(Show)
  
data OriginEnt = OBinary !ByteString
               | OText !Text
               | OTags [Tag Text]
               deriving(Show)

data YieldEnt = YBinary !ByteString
               | YText  !Text
               | YJson !Value
               | forall x. (NFData x, Typeable x) => YData !x
               | YUrl  !TaskDesc

instance Show YieldEnt where
  show y = "TODO"

data ResultEnt = RBinary !ByteString
               | RText !Text
               | RJson !Value
               | forall x. (NFData x, Typeable x) => RData !x
instance Show ResultEnt where
  show y = "TODO"
 
data DataFormat = RawBinary | UtfText | Tags deriving(Show)


newtype TaskData = TaskData { _tdDesc :: TaskDesc}

data SourceData = SourceData { _odDesc :: !TaskDesc
                             , _odMime :: !Text
                             , _odEntity :: !OriginEnt
                             }deriving(Show)

data YieldData = YieldData { _ydDesc :: !TaskDesc
                           , _ydEntity :: !YieldEnt
                           } deriving(Show)

data ResultData = ResultData{ _rdDesc :: !TaskDesc
                            , _rdEntity :: ResultEnt
                            }

-- data ScrahaParser = ValueSelector (TaskDesc -> Selector Value)
--                   | ValueMetaSelector (TaskDesc -> Selector (Value, Meta))
--                   | forall a. Typeable a => DataSelector (TaskDesc -> Selector a)
--                   | forall a. Typeable a => DataMetaSelector (TaskDesc -> Selector (a, Meta))

type ScraParser = SourceData -> IO [YieldData]
type ScraProcessor = ResultData -> IO ()

type StartsUrls = [(Text, Meta, Text)]

type ScraParserRec = (Text, ScraParser, DataFormat)
type ScraProcessorRec = (Text, ScraProcessor)

class ScraData a where
  taskdesc :: a -> TaskDesc
  urlOf :: a -> Text
  metaOf_ :: a -> Meta
  bodyAsBinary :: a -> ByteString
  bodyAsText :: a -> Text

data SessionData = forall hs. (Show hs, Typeable hs) => SessionData hs

taskDesc :: SourceData -> TaskDesc
taskDesc (SourceData td _ _) = td

metaOf :: SourceData -> Meta
metaOf = _tdMeta . _odDesc
                       
originText :: SourceData -> Text
originText d
  | OText t <- _odEntity d  = t
  | otherwise  = error "not contain text data"

originBytes :: SourceData -> ByteString
originBytes d = case _odEntity d of
                  OBinary b -> b
                  otherwise -> error "not contain ByteString"

originMime :: SourceData -> Text
originMime = _odMime

originTags :: SourceData -> [TextTag]
originTags d = case _odEntity d of
                 OTags tx -> tx
                 otherwise -> error "originData not contain Tags"

yieldJson :: SourceData -> Value -> YieldData
yieldJson od v = YieldData (taskDesc od) (YJson v)

yieldText :: SourceData -> Text -> YieldData
yieldText od text = YieldData (taskDesc od) (YText text)

yieldBinary :: SourceData -> ByteString -> YieldData
yieldBinary od b = YieldData (taskDesc od) (YBinary b)

yieldUrl :: SourceData -> Text -> Meta -> Text -> YieldData
yieldUrl od url meta hn = YieldData (taskDesc od) (YUrl $ TaskDesc url meta hn)

yieldUrl' :: SourceData -> Text -> Text -> YieldData
yieldUrl' od url hn = YieldData (taskDesc od) (YUrl $ TaskDesc url empty hn)

yieldData :: (NFData x, Typeable x) => SourceData -> x -> YieldData
yieldData od x = YieldData (taskDesc od) (YData x)

resultValue :: ResultData -> Value
resultValue rd = case _rdEntity rd of
  RJson v -> v
  otherwise -> error "result data is not a value data"

resultData :: (NFData x, Typeable x) => ResultData -> x
resultData rd = case _rdEntity rd of
  RData v -> case cast v of
    Just x -> x
    _ -> error "can not convert to taget data type"
  _ -> error "result data is not a poly data"
  
-- metaLookup:: Octopus a => a -> Text -> Maybe Text
-- metaLookup a k = lookup k (metaOf a)

--
instance NFData TaskDesc where
  rnf (TaskDesc url meta handler) =
    rnf url `seq` rnf meta `seq` rnf handler

instance NFData DataFormat where
  rnf x = ()

instance NFData OriginEnt where
  rnf (OBinary b) = rnf b
  rnf (OText t) = rnf t

instance NFData YieldEnt where
  rnf (YBinary b) = rnf b
  rnf (YText t) = rnf t
  rnf (YJson v) = rnf v
  rnf (YUrl td) = rnf td 
  rnf (YData d) = rnf d

instance NFData ResultEnt where
  rnf (RBinary b) = rnf b
  rnf (RText t) = rnf t
  rnf (RJson v) = rnf v

instance NFData TaskData where
  rnf (TaskData td) = rnf td

instance NFData SourceData where
  rnf (SourceData td mime ent)
    = rnf td `seq` rnf mime `seq` rnf ent

instance NFData YieldData where
  rnf (YieldData td ent) =  rnf td `seq` rnf ent

instance NFData ResultData where
  rnf (ResultData td ent) = rnf td `seq` rnf ent

timing :: NFData a => IO a -> IO (a, NominalDiffTime)
timing p = do
  t0 <- getCurrentTime
  r <- p
  t1 <- r `deepseq` getCurrentTime
  return (r, diffUTCTime t1 t0)

timing' :: IO a -> IO (a, NominalDiffTime)
timing' p = do
  t0 <- getCurrentTime
  !r <- p
  t1 <- getCurrentTime
  return (r, diffUTCTime t1 t0)
