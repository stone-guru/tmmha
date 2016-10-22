{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module SelectorTest where

import Test.HUnit

import TMM.Types
import TMM.Selector2

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
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

allTests = TestList [testAt, p20Tests, testNths, testLasts]

testAt = TestCase ( do  (i, n) <- runP20Sel nameAndId
                        T.putStrLn i
                        T.putStrLn n
                        assertEqual "name" "霖小兔" n
                        assertEqual "id"  "717117822" i)
  where
    nameAndId = at ".personal-info .top " $
      (,) <$> attrOf ".friend-follow" "data-userid" <*> textOf ".lady-name"

testAtFail = TestCase ( do  src <- runP20Sel notThisImage
                            assertEqual "src" "xxx" src)
  where
    notThisImage = at ".personal-info .top " $ attrOf "img" "src"

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

testNth3 = TestLabel "Nth in first for photos" $
  (TestCase $ do
      n1 <- runP20Sel $ at ".list-item .list-info" (textOf "li:nth-child(3) strong")
      T.putStrLn n1
      assertEqual "num of photo" "193" n1
  )
  
testNth4 = TestLabel "Nth in many for photos" $
  (TestCase $ do
      nx <- runP20Sel $ many ".list-item .list-info" (textOf "li:nth-child(3) strong")
      mapM_ T.putStrLn nx
      assertEqual "length" 10 (length nx)
      let n1:n2:n3:_ = nx
      assertEqual "num of photo" "193" n1
      assertEqual "num of photo" "112" n2
      assertEqual "num of photo" "100" n3
  )

testLast1 = TestLabel "testLast1" $
  (TestCase $ do
      s <- runM8509Sel $ at ".mm-p-base-info li:last-child" (textOf "p")
      T.putStrLn s
      assertEqual "experience" "34码" s)

testLast2 = TestLabel "last in many for signed contracts" $
  (TestCase $ do
      nx <- runP20Sel $ many ".list-info li:last-child" (textOf "strong")
      mapM_ T.putStrLn nx
      assertEqual "length" 10 (length nx)
      let n1:n2:n3:_ = nx
      assertEqual "num of photo" "0" n1
      assertEqual "num of photo" "73" n2
      assertEqual "num of photo" "33" n3
  )

testLast3 = TestLabel "2 level last in many for signed contracts" $
  (TestCase $ do
      nx <- runP20Sel $ many ".list-item div:last-child li:last-child" (textOf "strong")
      mapM_ T.putStrLn nx
      assertEqual "length" 10 (length nx)
      let n1:n2:n3:_ = nx
      assertEqual "num of photo" "0" n1
      assertEqual "num of photo" "73" n2
      assertEqual "num of photo" "33" n3
  )


testLast4 = TestLabel "3level last in for signed contracts" $
  (TestCase $ do
      let s = html2Test $ at "div:last-child div:last-child li:last-child" (textOf "strong")
      T.putStrLn s
      assertEqual "Signed contracts of last girl" "6" s
  )

testNthLast1 = TestLabel "nth last test 1" $
  (TestCase $ do
      let s = html2Test $ at "#div11 li:nth-last-child(2)" (textOf "p")
      T.putStrLn s
      assertEqual "li13 text" "item 3" s
  )

testLasts = TestList [testLast1, testLast2, testLast3, testLast4, testNthLast1]
  
html2Test sel = let ctx = initContext $ parseTags html2
                in evalSelect sel ctx
  where
    html2 = T.concat [
       "<div id=\"div0\">"
      ,  "<div id=\"div1\">"
      ,   "<div> </div>"
      ,   "<div> </div>"
      ,   "<div id=\"div11\">"
      ,      "<ul id=\"ul1\">"
      ,       "<li id=\"li11\">list item 1</li>"
      ,       "<li id=\"li12\">list item 2</li>"
      ,       "<li id=\"li13\"><p>item 3</p></li>"
      ,       "<li id=\"li14\"><strong>6</strong></li>"
      ,      "</ul>"
      ,      "<p> some text </p>"
      ,   "</div>" --div11
      ,  "</div>" -- div1
      , "</div>" --div0
      ]

testNths = TestList [testNth1, testNth2, testNth3, testNth4]

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

childCountTest1 = TestLabel "childCount1" $
  (TestCase $ do
      let tags = parseTags ccHtml1
          tagsn@((t1, sn1):rest) = zip tags [1..]
      let (v, cm) = childCount [(t1, sn1, 1)] rest 0 IntMap.empty
      -- putStrLn $ IntMap.showTree cm
      assertEqual "div1" 2 (cm IntMap.! 1)
      assertEqual "div11" 2 (cm IntMap.! 2)
      assertEqual "ul1" 4 (cm IntMap.! 3)
      assertEqual "li11" 0 (cm IntMap.! 4)
      assertEqual "a href" 1 (cm IntMap.! 21)
      --mapM_ (putStrLn . show ) $ map (\(t, sn) -> (t, sn, IntMap.findWithDefault 0 sn cm)) tagsn
      assertBool "tag text should not record" $ IntMap.notMember 18 cm
      )

ccHtml1 = T.concat [
  "<div id=\"div1\">"
  ,"<div id=\"div11\">"
  , "<ul id=\"ul1\">"
  ,    "<li id=\"li11\">list item 1</li>"
  ,    "<li id=\"li12\">list item 2</li>"
  ,    "<li id=\"li13\">list item 3</li>"
  ,    "<li id=\"li14\">list item 4</li>"
  ,  "</ul>"
  ,  "<p> some text </p>"
  , "</div>"
  , "<a href='blank'> <p>link text</p> </a>"
  , "</div>"
  ]
                     
