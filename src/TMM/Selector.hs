-- -*- coding: utf-8 -*-
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module TMM.Selector(
  css,
  css1,
  extract,
  bodyText,
)where
  
import qualified Data.Text as T
import Data.Text(Text)
import Text.HTML.TagSoup
import Debug.Trace
import Control.Exception
import Data.Maybe
import Debug.Trace

type Matcher a b = a -> b -> Bool
type Pred a = a -> Bool

data TagCondition = Ename Text | Cname Text |EId Text deriving (Show)

tagMatch :: Tag Text -> TagCondition -> Bool
tagMatch (TagOpen ts _ )   (Ename ns)  =  ns == ts -- trace "#tagOpen-name" $
tagMatch (TagOpen _ attrs) (EId ids)  =  -- trace "#tagOpen-id" $
                                         case lookup "id"  attrs of
                                           Nothing -> False
                                           Just s -> s == ids
tagMatch (TagOpen _ attrs) (Cname cs)  = -- trace "#tagOpen-class" $
                                          case lookup "class" attrs of
                                            Nothing -> False
                                            Just s -> elem cs (T.words s)
tagMatch _ _ = False

strToCond :: Text -> [TagCondition]
strToCond s = map f $ T.words s
  where
    f st = case (T.head st) of
      '.' -> tl `seq` Cname tl
      '#' -> tl `seq` EId tl
      _ -> Ename st
      where tl = T.tail st

data Comporator a b = Comporator (Pred a) (Pred a) (Matcher a b)
type Picker a = [Tag Text] -> a

tagComporator = Comporator isTagOpen isTagClose tagMatch

css ::  Text -> Picker a -> [Tag Text] -> [a]
css str f tags = _select tagComporator (strToCond str) False f tags

css1 ::  Text -> Picker a -> [Tag Text] -> [a]
css1 str f tags = _select tagComporator (strToCond str) True f tags

_select :: (Show a, Show b) => Comporator a b -> [b] -> Bool -> ([a] -> c) -> [a] ->[c]
_select _ [] _ _ _ = error "no condition given"
_select (Comporator isOpen isClose p) bx single f ax = loop [] ax
  where
    bxv = reverse bx

    loop !sx !ax = case search sx ax of
                   Nothing-> []
                   Just (stack, t:ts) -> (:) (f (t:ts))  $ if single
                                                           then [] else loop (t:stack) ts

    search _ [] = Nothing
    search !stack !rest@(a:ax)
      | isOpen a = if match p (a:stack) bxv -- trace "#match " $ 
                   then Just (stack, rest)
                   else search (a:stack) ax
      | isClose a = case stack of
                      _:sx -> search sx ax
                      [] -> error "unbalanced sequence"
      | otherwise = search stack ax


extract :: Text -> [Tag Text] -> Text
extract str tags = case css1 str bodyText tags of
                     t:_ -> t
                     [] -> T.empty

bodyText :: [Tag Text] -> Text
bodyText [] = T.empty
bodyText (t:tx) = T.concat $! loop [t] tx
  where
    loop _ [] = []
    loop [] _ = []
    loop !sx !((TagText s):tx) =  s : loop sx tx
    loop !(s:sx) !(t:tx)
      |isTagOpen t = loop (t:s:sx) tx
      |isTagClose t = loop sx tx
      |otherwise = loop (s:sx) tx

{-# INLINE match #-}
match :: Matcher a b -> [a] -> [b] -> Bool
match _ [] _  = False  -- path is empty
match _ _ []  = error "condition should not empty"
match p (a:ax) (b:bx) 
  | p a b = lookupper ax bx
  | otherwise =  False
  where
    lookupper _ [] = True
    lookupper [] _ = False
    lookupper !ax !(b:bx) = case dropWhile (\a -> not $! p a b) ax of
                              [] -> False
                              _:rest -> lookupper rest bx
                                     

data Regexy = StartsWith
            |   EndsWith
            |   Contains
            |   Equals
            |   Anything
            |   ContainsWord
            |   NotEquals
            deriving (Eq, Show)

data Selector =  Any
              |  Type Text
              |  Id Text
              |  Class Text
              |  Attribute  Regexy Text Text
              |  And [Selector]
              |  Or  [Selector]
              |  Not [Selector]
              |  Has [Selector]
              |  ParentIs Selector
              |  AncestorIs Selector
              
