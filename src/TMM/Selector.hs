-- -*- coding: utf-8 -*-
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
module TMM.Selector
where

import qualified Data.Text as T
import Data.Text(Text)
import Text.HTML.TagSoup
import qualified Text.HTML.TagSoup.Fast as F
import Control.Exception
import Data.Maybe
import Debug.Trace
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.ICU.Convert as ICU  -- text-icu

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
                                            Just s -> cs `elem` T.words s
tagMatch _ _ = False

strToCond :: Text -> [TagCondition]
strToCond s = map f $ T.words s
  where
    f st = case T.head st of
      '.' -> tl `seq` Cname tl
      '#' -> tl `seq` EId tl
      _ -> Ename st
      where tl = T.tail st

data Comporator a b = Comporator (Pred a) (Pred a) (Matcher a b)
type Picker a = [Tag Text] -> a

tagComporator = Comporator isTagOpen isTagClose tagMatch

css ::  Text -> Picker a -> [Tag Text] -> [a]
css str = _select tagComporator (strToCond str) False

css1 ::  Text -> Picker a -> [Tag Text] -> [a]
css1 str  = _select tagComporator (strToCond str) True

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
    search !stack rest@(a:ax)
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
    loop !sx (TagText s:tx) =  s : loop sx tx
    loop (s:sx) (t:tx)
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
    lookupper !ax (b:bx) = case dropWhile (\a -> not $! p a b) ax of
                              [] -> False
                              _:rest -> lookupper rest bx


type TagList = [Tag Text]
data SContext n c = SContext { _stack  :: [n]
                             , _crits :: [c]
                             , _rest :: [n]
                             , _cmp :: Comporator n c}

type TagContext = SContext (Tag Text) TagCondition

initContext :: [Tag Text] -> TagContext
initContext (t:tx) = SContext [t] [] tx tagComporator

newtype Select n c a = Select{ runSelect :: SContext n c -> (a,SContext n c) }

type TagSelect = Select (Tag Text) TagCondition

evalSelect :: Select n c a -> SContext n c -> a
evalSelect s sc = fst (runSelect s sc)

instance Functor (Select n c) where
  fmap f s1 = Select $ \s -> let (x, s') = runSelect s1 s
                             in (f x, s')

instance Applicative (Select n c) where
  pure x = Select $ \s -> (x, s)
  (<*>) :: Select n c (a -> b) -> Select n c a -> Select n c b
  (Select fs) <*> (Select sa) = Select $ \s -> let (f, s1) = fs s
                                               in  let (x, s2) = sa s1
                                                   in (f x, s2)

instance Monad (Select n c) where
  return = pure
  (>>=) :: Select n c a -> (a -> Select n c b) -> Select n c b
  (Select rs) >>= f = Select $ \s0 ->
    let (x, s1) = rs s0
    in runSelect (f x) s1

iter :: SContext a b -> SContext a b
iter (SContext topStack critx ax cmp@(Comporator isOpen isClose p)) =
  SContext stack critx rest cmp
  where
    (stack, rest) = search topStack ax
    search stack []  = (stack, [])
    search !stack rest@(a:ax)
      | isOpen a = if match p (a:stack) critx
                   then (stack, rest)
                   else search (a:stack) ax
      | isClose a = case stack of
          _:tails -> search tails ax
          _ -> error "iter: unbalanced tags"
      | otherwise = search stack ax

    {-# INLINE match #-}
    match :: Matcher a b -> [a] -> [b] -> Bool
    match _ _ []  = True -- empty conditon means match any node
    match p (a:ax) (b:bx)
      | p a b = lookupper ax bx
      | otherwise =  False
      where
        lookupper _ [] = True -- all condition matchs
        lookupper [] _ = False
        lookupper !ax (b:bx) = case dropWhile (\a -> not $! p a b) ax of
                                  [] -> False
                                  _:rest -> lookupper rest bx

backtrace1 :: SContext a b -> SContext a b
backtrace1 (SContext stack bx ax cmp@(Comporator isOpen isClose _)) = SContext sx  bx rest cmp
  where
    (sx, rest) = go stack ax
    go _ [] = error "no more tags to backtrace"
    go [] rest = case stack of
      [_] -> ([], rest)
      _ -> error "go up too much in backtrace1"
    go (s:sx) (a:ax)
      | length stack == length sx + 2 = (s:sx, a:ax)
      | isOpen a = go (a:s:sx) ax
      | isClose a = go sx ax
      | otherwise = go (s:sx) ax

numComporator :: Comporator Int Int
numComporator = Comporator (>0) (<0) (==)

instance (Show a, Show b) => Show (SContext a b) where
  show (SContext sx cx ax _) = "SContext stack = " ++ show sx
                               ++ ", critx = " ++ show cx
                               ++ ", rest = " ++ show ax


root :: Select n c n
root = Select $ \sc@(SContext _ _ (h:_) _) -> (h, sc)

nodes :: Select n c [n]
nodes = Select $ \sc@(SContext _ _ current _) -> (current, sc)

path :: Select n c [n]
path = Select $ \sc@(SContext stack _ (h:_) _) -> (h:stack, sc)

loc :: [c] -> Select n c ()
loc critx = Select $ \sc@(SContext stack _ ax cmp) ->
                       ((), iter $ SContext stack critx ax cmp)

at :: T.Text -> TagSelect a  -> TagSelect a
at s p = stay $ desire' (reverse $ strToCond s) p

one :: T.Text -> TagSelect a -> TagSelect a
one = at

textOf :: T.Text -> TagSelect T.Text
textOf crit = at crit $ fmap bodyText nodes

attrOf :: T.Text -> T.Text -> TagSelect T.Text
attrOf crit name = at crit $ fmap (fromAttrib name) root

desire' ::  [c] -> Select n c a -> Select n c a
desire'  critx p = loc critx >> p

context :: Select n c (SContext n c)
context = Select $ \s -> (s, s)

setContext :: SContext n c -> Select n c ()
setContext  sc = Select $ const ((), sc)

rest :: Select n c [n]
rest = context >>= \(SContext _ _ ax _) -> return ax

end :: Select n c Bool
end = do
  b <- fmap null rest
  -- trace ("call end got " ++ show b) $ return b
  return b

stay :: Select n c a -> Select n c a
stay p = do
  sc <- context
  x <- p
  setContext sc
  return x

nextNode :: Select n c ()
nextNode = do
  SContext stack bx ax cmp@(Comporator isOpen _ _) <- context
  let (sx, rest) = go isOpen stack ax
  setContext $ SContext sx  bx rest cmp
  where
    go _ sx [] = (sx, [])
    go isOpen sx ax = case dropWhile (not.isOpen) ax of
                        a:rest -> (a:sx, rest)
                        [] -> (sx, [])

many' :: [c] -> Select n c a -> Select n c [a]
many' critx p =  stay loop
  where
    loop = do
      loc critx
      b <- end
      if b
        then return []
        else -- trace "many found one" $
        do
          x <- stay p
          nextNode
          rx <- loop
          return $ x:rx

many :: T.Text -> TagSelect a -> TagSelect [a]
many s = many' (reverse $ strToCond s)

select1 :: Select Int Int String
select1 = desire' [1, 2]
          (do
              v <- fmap (+300) root
              sx <- selx 100
              sy <- sely 283.7
              px <- fmap (map (*2)) path
              return $  sx ++ sy ++ " " ++ show v ++ ", " ++ show px
          )
  where
    selx x = return $ show x
    sely y = return $ show y

runSelector :: [Tag Text] -> TagSelect a -> a
runSelector tags sel = evalSelect sel $ initContext tags
  
parseText :: T.Text -> TagSelect a -> a
parseText txt sel = evalSelect sel $ initContext $ parseTags txt

parseBinary :: ICU.Converter -> B8.ByteString -> TagSelect a -> a
parseBinary cvt bytes sel =  evalSelect sel $ initContext tags
  where
    b2t = ICU.toUnicode cvt
    tags = map (textTag b2t) $ F.parseTags bytes

type B2T = B8.ByteString -> T.Text

textTag :: B2T -> Tag B8.ByteString -> Tag T.Text
textTag bst (TagOpen t a) = TagOpen (bst t) [(bst n, bst v) | (n,v) <- a]
textTag bst (TagClose t) = TagClose (bst t)
textTag bst (TagText t) = TagText (bst t)
textTag bst (TagComment t) = TagComment (bst t)
textTag bst (TagWarning t) = TagWarning (bst t)
textTag bst (TagPosition r c) = TagPosition r c

getB2t :: String -> IO B2T
getB2t s = do
  converter <- ICU.open s Nothing
  return $ b2t converter
  where
    b2t cvt bytes = ICU.toUnicode cvt bytes
    
