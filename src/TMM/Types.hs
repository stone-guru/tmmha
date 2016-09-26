-- -*- coding: utf-8 -*-
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module TMM.Types where

import Control.Concurrent.Chan (Chan)
import Data.Text (Text)
import Data.Aeson (Value)
import Data.HashMap.Strict (HashMap)
import Data.ByteString.Lazy.Char8 (ByteString)
import Control.DeepSeq 

type Meta = HashMap Text Text
data OriginEnt = OriginBinary !ByteString
               | OriginText !Text
               
data YieldEnt = YieldBinary !ByteString
               | YieldText  !Text
               | YieldJson !Value
               | YieldUrl  !String
               
data ResultEnt = ResultBinary !ByteString
               | ResultText !Text
               | ResultJson !Value

data TaskData = TaskData !String !Meta !String               
data OriginData = OriginData !String !Text !Meta OriginEnt 
data YieldData = YieldData !String !Meta YieldEnt
data ResultData = ResultData !String !Meta ResultEnt

class ExchangeData a where
  typeOf :: a -> String
  metaOf :: a -> Meta

type ShParser = OriginData -> IO [YieldData]
type ShProcessor = ResultData -> IO ()

type StartsUrls = [(String, String)]

class AsText t where
  asText :: t -> Text

originText :: OriginData -> Text
originText (OriginData _ _ _ (OriginText t)) = t
originText _ = error "not contain text data"

originBytes :: OriginData -> ByteString
originBytes (OriginData _ _ _ (OriginBinary b)) = b
originBytes _ = error "not contain ByteString"

originMime :: OriginData -> Text
originMime (OriginData _ ct _ _) = ct

yieldData :: String -> Meta -> YieldEnt -> YieldData
yieldData t meta ent = YieldData t meta ent


yieldJson :: String -> Meta -> Value -> YieldData
yieldJson t meta v = YieldData t meta (YieldJson v)
  
yieldText :: String -> Meta -> Text -> YieldData
yieldText t meta text = YieldData t meta (YieldText text)

yieldBinary :: String -> Meta -> ByteString -> YieldData
yieldBinary t meta b = YieldData t meta (YieldBinary b)

yieldUrl :: String -> Meta -> String -> YieldData
yieldUrl t meta url = YieldData t meta (YieldUrl url)

-- metaLookup:: Octopus a => a -> Text -> Maybe Text
-- metaLookup a k = lookup k (metaOf a)

--
instance ExchangeData OriginData  where
  typeOf (OriginData t _ _ _) = t
  metaOf (OriginData _ _ m _) = m

instance ExchangeData YieldData  where
  typeOf (YieldData t _ _ ) = t
  metaOf (YieldData _ m _) = m

instance ExchangeData ResultData  where
  typeOf (ResultData t _ _) = t
  metaOf (ResultData _ m _) = m

instance NFData OriginEnt where
  rnf (OriginBinary b) = rnf b
  rnf (OriginText t) = rnf t

instance NFData YieldEnt where
  rnf (YieldBinary b) = rnf b
  rnf (YieldText t) = rnf t
  rnf (YieldJson v) = rnf v
  rnf (YieldUrl url) = rnf url
  
instance NFData ResultEnt where
  rnf (ResultBinary b) = rnf b
  rnf (ResultText t) = rnf t
  rnf (ResultJson v) = rnf v

  
instance NFData TaskData where
  rnf (TaskData t m url) = rnf t `seq` rnf m `seq` rnf url

instance NFData OriginData where
  rnf (OriginData t ct m e) = rnf t `seq` rnf ct `seq` rnf m `seq` rnf e

instance NFData YieldData where
  rnf (YieldData t m e) =  rnf t `seq` rnf m `seq` rnf e

instance NFData ResultData where
  rnf (ResultData t m e) = rnf t `seq` rnf m `seq` rnf e

  
