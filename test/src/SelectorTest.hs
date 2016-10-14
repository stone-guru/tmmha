{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module SelectorTest where

import Test.HUnit

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
import Data.Maybe
import System.Environment (getArgs)
import Data.Aeson
import Control.Applicative ((<$>), (<*>))
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import Debug.Trace

testAt = TestCase ( do  (i, n) <- runP20Sel nameAndId
                        assertEqual "name" "霖小兔" n
                        assertEqual "id"  "717117822" i)
  where
    nameAndId = at ".personal-info .top " $ 
      (,) <$> attrOf ".friend-follow" "data-userid" <*> textOf ".lady-name"
  
p20Tests = TestList [TestLabel "testAt" testAt]

testNth1 = TestLabel "testNth1" $
  (TestCase $ do
      d <- runM8509Sel (textOf ".mm-p-base-info li:nth-child(3) span")
      T.putStrLn d
      assertEqual "place" "赣州市" d)

testNth2 = TestLabel "testNth2" $
  (TestCase $ do
      s <- runM8509Sel $ at "div:nth-child(4)" (attrOf "h4" "title")
      T.putStrLn s
      assertEqual "experience" "经历" s)

testNth3 = TestLabel "Nth in many for photos" $
  (TestCase $ do
      nx <- runP20Sel $ many ".list-item .list-info" (textOf "li:nth-child(3) strong")
      mapM_ T.putStrLn nx
      assertEqual "length" 10 (length nx)
      let n1:n2:n3:_ = nx
      assertEqual "num of photo" "193" n1
      assertEqual "num of photo" "112" n2
      assertEqual "num of photo" "100" n3
  )

nthTests = TestList [testNth1, testNth2, testNth3]

page20 = "/home/bison/sources/haskell/tmmha/test/data/list-page-20.html"

runP20Sel :: Select a -> IO a
runP20Sel sel = do
  tags <- parseFile page20
  return $ evalSelect sel (initContext tags)

m8509 = "/home/bison/sources/haskell/tmmha/test/data/model_info_28168509.html"
  
runM8509Sel :: Select a -> IO a
runM8509Sel sel = do
  tags <- parseFile m8509
  return $ evalSelect sel (initContext tags)
  
parseFile :: String -> IO [TextTag]
parseFile fn = do
  cxt <- T.readFile fn
  return $ parseTags cxt
