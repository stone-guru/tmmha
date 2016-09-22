-- -*- coding: utf-8 -*-
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module TMM.Selector where
import qualified Data.Text as T
import Text.HTML.TagSoup
import Debug.Trace

data Selector a = Fun (a -> Selector a) | OK | No
type Pred a b = b -> a -> Bool
type Accept a = a -> Bool

data TagCondition = Ename T.Text | Cname T.Text deriving (Show)

tagMatch :: TagCondition -> Tag T.Text -> Bool
tagMatch (Ename ns) (TagOpen ts _ ) = ns == ts
tagMatch (Ename _ ) _ = False
tagMatch (Cname cs) (TagOpen _ attrs) = case lookup "class" attrs of
                                          Nothing -> False
                                          Just s -> elem cs (T.words s)

strToCond :: T.Text -> [TagCondition]
strToCond s = map f $ T.words s
  where
    f st = if '.' == (T.head st)
           then Cname $ T.tail st
           else Ename st

select :: T.Text -> [Tag T.Text] -> [[Tag T.Text]]
select str tags = _select (strToCond str) isTagOpen isTagClose tagMatch tags

select1 :: T.Text -> [Tag T.Text] -> [Tag T.Text]
select1 str tags = head $ select str tags

extract :: T.Text -> [Tag T.Text] -> T.Text
extract str tags = bodyText $ select1 str tags

bodyText :: [Tag T.Text] -> T.Text
bodyText [] = T.empty
bodyText (t:tx) = T.concat $ loop [t] tx
  where
    loop _ [] = []
    loop [] _ = []
    loop sx ((TagText s):tx) =  s : loop sx tx
    loop (s:sx) (t:tx)
      |isTagOpen t = loop (t:s:sx) tx
      |isTagClose t = loop sx tx
      |otherwise = loop (s:sx) tx

_select :: (Show a, Show b) => [b] -> Accept a -> Accept a -> Pred a b -> [a] ->[[a]]
_select [] _ _ _ _ = error "no condition given"
_select bx isOpen isClose p ax = loop [] ax
  where
    bxv = reverse bx

    loop sx ax = case search sx ax of
                   Nothing-> []
                   Just (stack, t:ts) -> (t:ts) : loop (t:stack) ts

    search _ [] = Nothing
    search stack rest@(a:ax)
      | isOpen a = if match p bxv (a:stack) -- trace ((show bxv) ++ ", " ++ (show (a:stack)))
                   then Just (stack, rest)
                   else search (a:stack) ax
      | isClose a = case stack of
                      _:sx -> search sx ax
                      [] -> error "unbalanced sequence"
      | otherwise = search stack ax

match :: Pred a b -> [b] -> [a] -> Bool
match _ _ []  = False
match _ [] _  = error "condition should not empty"
match p (b:bx) (a:ax)
  | p b a = lookupper bx ax
  | otherwise =  False
  where
    lookupper [] _ = True
    lookupper _ [] = False
    lookupper (b:bx) ax = case dropWhile (not.(p b)) ax of
                              [] -> False
                              _:rest -> lookupper bx rest
