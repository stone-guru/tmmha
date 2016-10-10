-- -*- coding: utf-8 -*-
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
               | YUrl  !TaskDesc
               deriving(Show)
               
data ResultEnt = RBinary !ByteString
               | RText !Text
               | RJson !Value
               deriving(Show)

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

type ScraParser = SourceData -> IO [YieldData]
type ScraProcessor = ResultData -> IO ()

type StartsUrls = [(Text, Text)]

type ScraParserRec = (Text, ScraParser, DataFormat)
type ScraProcessorRec = (Text, ScraProcessor)

class ScraData a where
  taskdesc :: a -> TaskDesc
  urlOf :: a -> Text
  metaOf_ :: a -> Meta
  bodyAsBinary :: a -> ByteString
  bodyAsText :: a -> Text

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

resultValue :: ResultData -> Value
resultValue rd = case _rdEntity rd of
  RJson v -> v
  otherwise -> error "result data is not a value data"
  
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
