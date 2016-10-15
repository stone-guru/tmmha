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

data Context = Context { _stack  :: [(Textag, Int)]
                       , _rest :: [Textag]
                       , _nexti :: Int
                       }

instance Show Context  where
  show (Context sx ax n) = "Context stack = " ++ show sx
                           ++ ", rest = " ++ show ax
                           ++ ", nexti = " ++ show n

newtype Select a = Select {runSelect :: Context -> (a, Context)}

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

type Matcher = Textag -> Int -> Criterion -> Bool
type Pred = Textag -> Bool
data Comporator = Comporator Pred Pred Matcher

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

evalSelect :: Select a -> Context -> a
evalSelect s sc = fst (runSelect s sc)

initContext :: [Textag] -> Context
initContext tx = Context [(beginTag, 1)] (regular tx) 1
  where
    beginTag = TagOpen "//" []
    regular [] = TagClose "//" : []
    regular (a:ax) = a : (regular ax)

cget :: (Context -> a) -> Select a
cget f = Select $ \sc -> (f sc, sc)

cmodify :: (Context -> Context) -> Select ()
cmodify f = Select $ \sc -> ((), f sc)

context :: Select Context
context = Select $ \s -> (s, s)

root :: Select Textag
root = fmap (fst.head) $ cget _stack

nodes :: Select [Textag]
nodes = do
  h <- root
  fmap ((:) h) (cget _rest)

path :: Select [Textag]
path = fmap (map fst) (cget _stack)

loc :: [Criterion] -> Select Bool
loc critx = Select $ \sc -> seek critx sc

seek :: [Criterion] -> Context -> (Bool, Context)
seek critx (Context topStack ax nexti) =
  -- trace (show stack ++ ", " ++ show (take 5 rest)) $
  (found, Context stack rest i)
  where
    (found, stack, i, rest) = search topStack nexti ax 
    search [] _ _  = (False, topStack, nexti, ax) -- stack empty, means not found within current sub tree
    search _ _ [] = (False, topStack, nexti, ax) -- no more nodes
    search !stack i (a:ax)
      | isTagOpen a = let stack' = (a, i):stack
                   in if match stack' critx
                      then (True, stack', 1, ax)
                      else search stack' 1  ax
      | isTagClose a = case stack of
          (_, k):tails -> search tails (k+1) ax
          _ -> error "iter: inner error stack should not be empty at here"
      | otherwise = search stack i ax

    {-# INLINE match #-}
    match :: [(Textag, Int)] -> [Criterion] -> Bool
    match _ []  = True -- empty conditon means match any node
    match ((a, i):ax) (b:bx)
      | critMatch a i b = lookupper ax bx
      | otherwise =  False
      where
        lookupper _ [] = True -- all condition matchs
        lookupper [] _ = False
        lookupper !ax (b:bx) = case dropWhile (\(a, i) -> not $! critMatch a i b) ax of
                                  [] -> False
                                  _:rest -> lookupper rest bx

at :: Text -> Select a -> Select a
at s p = stay $ do
  found <- loc (reverse $ parseCrit s)
  if found
    then restrict >>  p
    else error $ show s ++ " not found"

restrict :: Select ()
restrict = Select $ \sc ->
  let stack  = case  _stack sc of
                 s:_ -> [s]
                 [] -> []
  in  ((), sc{_stack = stack})

textOf :: Text -> Select Text
textOf crit = at crit $ fmap bodyText nodes

attrOf :: Text -> Text -> Select Text
attrOf crit name = at crit $ fmap (fromAttrib name) root

searchText :: Text -> Text -> Select [Text]
searchText crit pat = do
  s <- textOf crit
  let (_, _, _, r) = s =~ pat :: (Text, Text, Text, [Text])
  return r

stay :: Select a -> Select a
stay p = do
  sc <- context
  x <- p
  cmodify $ const sc
  return x

many :: Text -> Select a -> Select [a]
many s p = stay $ loop []
  where
    critx = reverse $ parseCrit s
    loop rx = do
      b <- loc critx
      if not b
        then return $ reverse rx
        else do
          x <- stay $ restrict >> p
          loop (x:rx)

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

runSelector :: [Textag] -> Select a -> a
runSelector tags sel = evalSelect sel $ initContext tags

parseText :: Text -> Select a -> a
parseText txt sel = runSelector (parseTags txt)  sel

parseBinary :: ICU.Converter -> B8.ByteString -> Select a -> a
parseBinary cvt bytes sel =  evalSelect sel $ initContext tags
  where
    b2t = ICU.toUnicode cvt
    tags = map (textTag b2t) $ F.parseTags bytes

type B2T = B8.ByteString -> Text

textTag :: B2T -> Tag B8.ByteString -> Tag Text
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
