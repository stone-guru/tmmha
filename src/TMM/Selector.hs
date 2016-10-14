-- -*- coding: utf-8 -*-
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
module TMM.Selector
where

import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Text(Text)
import Text.HTML.TagSoup
import qualified Text.HTML.TagSoup.Fast as F
import Control.Exception
import Data.Maybe
import Debug.Trace
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.ICU.Convert as ICU  -- text-icu
import Text.Regex.TDFA
import Text.Regex.TDFA.Text

data Criterion = Criterion EntCrit AdCrit deriving (Eq, Show)

data EntCrit = EName Text       -- ^ Entity name, such as <p> <div>
             | CName Text       -- ^ Class name, such <x class="mm-p-info">
             | EId Text         -- ^ Id criterion , for <x id="some">
             deriving (Eq, Show)

data AdCrit = AdNone             -- ^ No addon condition
            | FirstChild         -- ^ First child of its parent
            | NthChild Int       -- ^ Nth child of its parent
            deriving (Eq, Show)

type Textag = Tag Text

type Matcher = Textag -> Int -> Criterion -> Bool
type Pred = Textag -> Bool
data Comporator = Comporator Pred Pred Matcher

data SContext = SContext { _stack  :: [(Textag, Int)]
                         , _crits :: [Criterion]
                         , _rest :: [Textag]
                         , _nexti :: Int
                         , _cmp :: Comporator}

instance Show SContext  where
  show (SContext sx cx ax n _) = "SContext stack = " ++ show sx
                                 ++ ", critx = " ++ show cx
                                 ++ ", rest = " ++ show ax
                                 ++ ", nexti = " ++ show n

newtype Select a = Select{ runSelect :: SContext -> (a, SContext) }

instance Functor Select where
  fmap f s1 = Select $ \s -> let (x, s') = runSelect s1 s
                             in (f x, s')

instance Applicative Select  where
  pure x = Select $ \s -> (x, s)
  (<*>) :: Select (a -> b) -> Select a -> Select b
  (Select fs) <*> (Select sa) = Select $ \s -> let (f, s1) = fs s
                                               in  let (x, s2) = sa s1
                                                   in (f x, s2)

instance Monad Select where
  return = pure
  (>>=) :: Select a -> (a -> Select b) -> Select b
  (Select rs) >>= f = Select $ \s0 ->
    let (x, s1) = rs s0
    in runSelect (f x) s1

tagComporator :: Comporator
tagComporator = Comporator isTagOpen isTagClose critMatch

critMatch :: Textag -> Int -> Criterion -> Bool
critMatch t i (Criterion entCrit adCrit) = tagMatch t entCrit && adMatch adCrit i
  where
    tagMatch :: Textag -> EntCrit -> Bool
    tagMatch (TagOpen ts _ ) (EName ns)  = ns == ts -- trace "#tagOpen-name" $
    tagMatch (TagOpen _ attrs) (EId ids) =  -- trace "#tagOpen-id" $
      case lookup "id"  attrs of
        Nothing -> False
        Just s -> s == ids
    tagMatch (TagOpen _ attrs) (CName cs) = -- trace "#tagOpen-class" $
      case lookup "class" attrs of
        Nothing -> False
        Just s -> cs `elem` T.words s
    tagMatch _ _ = False
    
    adMatch :: AdCrit -> Int -> Bool
    adMatch AdNone _ = True
    adMatch FirstChild i = i == 1
    adMatch (NthChild n) i = i == n

parseCrit :: Text -> [Criterion]
parseCrit s = map f $ T.words s
  where
    f st = case T.head st of
             '.' -> crit CName (T.tail st) 
             '#' -> crit EId (T.tail st)
             _ -> crit EName st
    crit cf s | (s1, s2) <- T.breakOn ":" s = Criterion (cf s1) (adcrit s2)
    adcrit "" = AdNone
    adcrit ads
      | ads == ":first-child" = FirstChild
      | Just s <- T.stripPrefix ":nth-child(" ads =  case T.decimal s of
                                                       Right (i, ")") -> NthChild i
                                                       other -> wrong $ show other
     | otherwise = wrong $ T.unpack ads
    wrong reason = error $ "unrecongnized tag selector " ++ reason

iter :: SContext -> SContext
iter (SContext topStack critx ax nexti cmp@(Comporator isOpen isClose p)) =
  -- trace (show stack ++ ", " ++ show (take 5 rest)) $
  SContext stack critx rest i cmp
  where
    (stack, i, rest) = search topStack nexti ax
    search stack i []  = (stack, i, [])
    search !stack i (a:ax)
      | isOpen a = let stack' = (a, i):stack
                   in if match p stack' critx
                      then (stack, i, a:ax)
                      else search stack' 1  ax
      | isClose a = case stack of
          (_, k):tails -> search tails (k+1) ax
          _ -> error "iter: unbalanced tags"
      | otherwise = search stack i ax

    {-# INLINE match #-}
    match :: Matcher -> [(Textag, Int)] -> [Criterion] -> Bool
    match _ _ []  = True -- empty conditon means match any node
    match p ((a, i):ax) (b:bx)
      | p a i b = lookupper ax bx
      | otherwise =  False
      where
        lookupper _ [] = True -- all condition matchs
        lookupper [] _ = False
        lookupper !ax (b:bx) = case dropWhile (\(a, i) -> not $! p a i b) ax of
                                  [] -> False
                                  _:rest -> lookupper rest bx

evalSelect :: Select a -> SContext -> a
evalSelect s sc = fst (runSelect s sc)

initContext :: [Textag] -> SContext
initContext tx = SContext [] [] tx 1 tagComporator


nodes :: Select [Textag]
nodes = Select $ \sc -> (_rest sc, sc)

root :: Select Textag
root = fmap head nodes

path :: Select [Textag]
path = Select $ \sc -> let cur =  head (_rest sc)
                           stacks = map fst (_stack sc)
                       in (cur:stacks, sc)

loc :: [Criterion] -> Select ()
loc critx = Select $ \sc -> ((), iter $ sc{_crits = critx})

at :: T.Text -> Select a -> Select a
at s p = stay $ desire' (reverse $ parseCrit s) p

one :: T.Text -> Select a -> Select a
one = at

textOf :: T.Text -> Select T.Text
textOf crit = at crit $ fmap bodyText nodes

attrOf :: T.Text -> T.Text -> Select T.Text
attrOf crit name = at crit $ fmap (fromAttrib name) root

searchText :: T.Text -> T.Text -> Select [T.Text]
searchText crit pat = do
  s <- textOf crit
  let (_, _, _, r) = s =~ pat :: (Text, Text, Text, [Text])
  return r

desire' :: [Criterion] -> Select a -> Select a
desire'  critx p = loc critx >> p

context :: Select SContext
context = Select $ \s -> (s, s)

setContext :: SContext -> Select ()
setContext  sc = Select $ const ((), sc)

rest :: Select [Textag]
rest = context >>= \sc -> return $ _rest sc

end :: Select Bool
end = do
  b <- fmap null rest
  -- trace ("call end got " ++ show b) $ return b
  return b

stay :: Select a -> Select a
stay p = do
  sc <- context
  x <- p
  setContext sc
  return x

nextNode :: Select ()
nextNode = do
  SContext stack bx ax n cmp@(Comporator isOpen isClose _) <- context
  let (sx, rest, i) = go isOpen isClose stack ax i
  setContext $ SContext sx bx rest i cmp
  where
    go _ _ sx [] i = (sx, [], i)
    go isOpen isClose sx (a:ax) i
      | isOpen a = ((a, i):sx, ax, 1)
      | isClose a = case sx of
          (t, k):sx1 -> go isOpen isClose sx1 ax (k + 1)
          [] -> error $ "nextNode unbalanced tree when isClose a"
      | otherwise = go isOpen isClose sx ax i
      
      -- case dropWhile (not.isOpen) ax of
      --                   a:rest -> (a:sx, rest)
      --                   [] -> (sx, [])

many :: T.Text -> Select a -> Select [a]
many s = many' (reverse $ parseCrit s)

many' :: [Criterion] -> Select a -> Select [a]
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

runSelector :: [Textag] -> Select a -> a
runSelector tags sel = evalSelect sel $ initContext tags
  
parseText :: T.Text -> Select a -> a
parseText txt sel = runSelector (parseTags txt)  sel

parseBinary :: ICU.Converter -> B8.ByteString -> Select a -> a
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
    
-- old implementations
-- css ::  Text -> Picker a -> [Tag Text] -> [a]
-- css str = _select tagComporator (parseCrit str) False

-- css1 ::  Text -> Picker a -> [Tag Text] -> [a]
-- css1 str  = _select tagComporator (parseCrit str) True

-- _select :: (Show a, Show b) => Comporator a b -> [b] -> Bool -> ([a] -> c) -> [a] ->[c]
-- _select _ [] _ _ _ = error "no condition given"
-- _select (Comporator isOpen isClose p) bx single f ax = loop [] ax
--   where
--     bxv = reverse bx

--     loop !sx !ax = case search sx ax of
--                    Nothing-> []
--                    Just (stack, t:ts) -> (:) (f (t:ts))  $ if single
--                                                            then [] else loop (t:stack) ts

--     search _ [] = Nothing
--     search !stack rest@(a:ax)
--       | isOpen a = if match p (a:stack) bxv -- trace "#match " $
--                    then Just (stack, rest)
--                    else search (a:stack) ax
--       | isClose a = case stack of
--                       _:sx -> search sx ax
--                       [] -> error "unbalanced sequence"
--       | otherwise = search stack ax


bodyText :: [Textag] -> Text
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

-- {-# INLINE match #-}
-- match :: Matcher a b -> [a] -> [b] -> Bool
-- match _ [] _  = False  -- path is empty
-- match _ _ []  = error "condition should not empty"
-- match p (a:ax) (b:bx)
--   | p a 0 b = lookupper ax bx
--   | otherwise =  False
--   where
--     lookupper _ [] = True
--     lookupper [] _ = False
--     lookupper !ax (b:bx) = case dropWhile (\a -> not $! p a 0 b) ax of
--                               [] -> False
--                               _:rest -> lookupper rest bx

-- backtrace1 :: SContext a b -> SContext a b
-- backtrace1 (SContext stack bx ax cmp@(Comporator isOpen isClose _)) = SContext sx  bx rest cmp
--   where
--     (sx, rest) = go stack ax
--     go _ [] = error "no more tags to backtrace"
--     go [] rest = case stack of
--       [_] -> ([], rest)
--       _ -> error "go up too much in backtrace1"
--     go (s:sx) (a:ax)
--       | length stack == length sx + 2 = (s:sx, a:ax)
--       | isOpen a = go (a:s:sx) ax
--       | isClose a = go sx ax
--       | otherwise = go (s:sx) ax

-- extract :: Text -> [Tag Text] -> Text
-- extract str tags = case css1 str bodyText tags of
--                      t:_ -> t
--                      [] -> T.empty

-- select1 :: Select Int Int String
-- select1 = desire' [1, 2]
--           (do
--               v <- fmap (+300) root
--               sx <- selx 100
--               sy <- sely 283.7
--               px <- fmap (map (*2)) path
--               return $  sx ++ sy ++ " " ++ show v ++ ", " ++ show px
--           )
--   where
--     selx x = return $ show x
--     sely y = return $ show y
