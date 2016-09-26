-- -*- coding: utf-8 -*-
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module TMM.Selector(
  select,
  select1,
  extract,
  bodyText,
  css,
  css1
)where
  
import qualified Data.Text as T
import Text.HTML.TagSoup
import Debug.Trace
import Control.Exception
import Data.Maybe
import Debug.Trace

data Selector a = Fun (a -> Selector a) | OK | No
type Matcher a b = a -> b -> Bool
type Pred a = a -> Bool

data TagCondition = Ename T.Text | Cname T.Text |EId T.Text deriving (Show)

tagMatch :: Tag T.Text -> TagCondition -> Bool
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

strToCond :: T.Text -> [TagCondition]
strToCond s = map f $ T.words s
  where
    f st = case (T.head st) of
      '.' -> Cname $ T.tail st
      '#' -> EId $ T.tail st
      _ -> Ename st

data Comporator a b = Comporator (Pred a) (Pred a) (Matcher a b)
type Picker a = [Tag T.Text] -> a

tagComporator = Comporator isTagOpen isTagClose tagMatch

css ::  T.Text -> Picker a -> [Tag T.Text] -> [a]
css str f tags = _selectb tagComporator (strToCond str) False f tags

css1 ::  T.Text -> Picker a -> [Tag T.Text] -> [a]
css1 str f tags = _selectb tagComporator (strToCond str) True f tags

_selectb :: (Show a, Show b) => Comporator a b -> [b] -> Bool -> ([a] -> c) -> [a] ->[c]
_selectb _ [] _ _ _ = error "no condition given"
_selectb (Comporator isOpen isClose p) bx single f ax = loop [] ax
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


select :: T.Text -> [Tag T.Text] -> [[Tag T.Text]]
select str tags = _select (strToCond str) isTagOpen isTagClose tagMatch tags

select1 :: T.Text -> [Tag T.Text] -> [Tag T.Text]
select1 str tags = head $ select str tags

extract :: T.Text -> [Tag T.Text] -> T.Text
extract str tags = bodyText $ select1 str tags

bodyText :: [Tag T.Text] -> T.Text
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

_select :: (Show a, Show b) => [b] -> Pred a -> Pred a -> Matcher a b -> [a] ->[[a]]
_select [] _ _ _ _ = error "no condition given"
_select bx isOpen isClose p ax = loop [] ax
  where
    bxv = reverse bx

    loop !sx !ax = case search sx ax of
                   Nothing-> []
                   Just (stack, t:ts) -> (t:ts) : loop (t:stack) ts

    search _ [] = Nothing
    search !stack !rest@(a:ax)
      | isOpen a =  -- trace ((show $ a:stack) ++ ", " ++ (show bxv)) $
                   if match p (a:stack) bxv 
                   then Just (stack, rest)
                   else search (a:stack) ax
      | isClose a = case stack of
                      _:sx -> search sx ax
                      [] -> error "unbalanced sequence"
      | otherwise = search stack ax

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
                                     
