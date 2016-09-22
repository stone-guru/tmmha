-- -*- coding: utf-8 -*-
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module TMM.Types where

import Control.Concurrent.Chan (Chan)
import Data.Text (Text)
import Data.Aeson (Value)
import Data.HashMap.Strict (HashMap)

type Meta = HashMap Text Text

class DataStream a where
  isEnd :: a -> Bool
  endGuard :: a

data RequestTask = RequestTask !Meta !String
                 | TaskEnd
type TaskQueue = Chan RequestTask

data ResponseData = ResponseText !Meta !Text
                  | ResponseBin !Meta !Text
                  | ResponseEnd
type DataQueue = Chan ResponseData

data ParseResult = DataResult !Meta  !Value
                 | UrlResult !Meta !String
                 | ResultEnd
type ResultQueue = Chan ParseResult


instance DataStream RequestTask where
  isEnd TaskEnd = True
  isEnd _ = False
  endGuard = TaskEnd

instance DataStream ResponseData where
  isEnd ResponseEnd = True
  isEnd _ = False
  endGuard = ResponseEnd

instance DataStream ParseResult where
  isEnd ResultEnd = True
  isEnd _ = False
  endGuard = ResultEnd

type TmmParser = Meta -> Text -> [ParseResult]
type TmmProcessor = Meta -> Value -> IO ()

class AsText t where
  asText :: t -> Text

type StartsUrls = [(HashMap Text Text, String)]






