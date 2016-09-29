{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import TMM.Types
import TMM.Selector
  
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

type TmmTag = Tag T.Text
page20 = "/home/bison/sources/haskell/tmmha/test/data/a.html"
  
test1 = do
  tags <- parseFile page20
  printTuple $ evalSelect sel1 (initContext tags) 
  let sx = evalSelect sel2 (initContext tags)
  mapM_   printTuple sx
  where
    printTuple (s1, s2) = T.putStrLn s1 >> T.putStrLn s2
    sel1 = at ".personal-info .top " $ do
      s2 <- attrOf "data-userid" ".friend-follow" 
      s1 <- textOf ".lady-name"
      return (s1, s2)
  
    sel2 = many ".personal-info .top" $ do
      s2 <- attrOf "data-userid" ".friend-follow" 
      s1 <- textOf ".lady-name"
      return (s1, s2)
      
parseFile :: String -> IO [TmmTag]
parseFile fn = do
  cxt <- T.readFile fn
  return $ parseTags cxt
  
