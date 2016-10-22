-- -*- coding: utf-8 -*-
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
module TMM.Selector(
  Select(..),
  at,
  many,
  textOf,
  attrOf,
  runSelector,
  searchText,
  evalSelect,
  initContext,
  countChild
  )where

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
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Control.Monad
import qualified Data.List as L
import qualified Data.Vector as V

data Criterion = Criterion EntCrit AdCrit deriving (Eq, Show)

data EntCrit = EName Text       -- ^ Entity name, such as <p> <div>
             | CName Text       -- ^ Class name, such <x class="mm-p-info">
             | EId Text         -- ^ Id criterion , for <x id="some">
             deriving (Eq, Show)

data AdCrit = AdNone             -- ^ No addon condition
            | FirstChild         -- ^ First child of its parent
            | LastChild          -- ^ Last child of its parent
            | NthChild Int       -- ^ Nth child of its parent
            | NthLastChild Int   -- ^ Reverse order nth child of its parent
            deriving (Eq, Show)

type Textag = Tag Text

type MatchCandi = [Int]

type StackElem = (Textag, Int, Int)
type TagStack = [StackElem]
type TagList  = [(Textag, Int)]

data Context = Context { _stack  :: TagStack
                       , _rest :: TagList
                       , _nc :: Int
                       , _mcs :: [MatchCandi]
                       , _ccmap :: IntMap Int
                       }

instance Show Context  where
  show (Context sx ax n mc ccmap)
    = "Context stack = " ++ show sx
    ++ ", rest = " ++ show (take 3 ax)
    ++ ", nc = " ++ show n
    ++ ", ccmap = " ++ show ccmap
    ++ ", matchCandi = " ++ show mc

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

runSelector :: [Textag] -> Select a -> a
runSelector tags sel = evalSelect sel $ initContext tags

evalSelect :: Select a -> Context -> a
evalSelect s sc = fst (runSelect s sc)

at :: Text -> Select a -> Select a
at s p = stay $ do
  found <- goto (V.fromList $ parseCrit s)
  if found
    then restrict >>  p
    else error $ show s ++ " not found"

many :: Text -> Select a -> Select [a]
many s p = stay $ loop []
  where
    critx = V.fromList $ parseCrit s
    loop rx = do
      b <- goto critx
      if not b
        then return $ reverse rx
        else do
          x <- stay $ restrict >> p
          loop (x:rx)

root :: Select Textag
root = fmap (first.head) $ cget _stack

nodes :: Select [Textag]
nodes = do
  h <- root
  rest <- cget _rest
  return $ h : map fst rest

path :: Select [Textag]
path = fmap (map first) (cget _stack)

textOf :: Text -> Select Text
textOf crit = at crit $ fmap bodyText nodes

attrOf :: Text -> Text -> Select Text
attrOf crit name = at crit $ fmap (fromAttrib name) root

searchText :: Text -> Text -> Select [Text]
searchText crit pat = do
  s <- textOf crit
  let (_, _, _, r) = s =~ pat :: (Text, Text, Text, [Text])
  return r

initContext :: [Textag] -> Context
initContext tx = Context [(beginTag, 1, 1)] (zip (regular tx) [2..]) 0 [] IntMap.empty
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

goto :: V.Vector Criterion -> Select Bool
goto critv = Select $ \sc0 ->
  let (found, sc1) = loop sc0
  in (found, if found then sc1 else sc0{_ccmap = _ccmap sc1})
  where
    loop :: Context -> (Bool, Context)
    loop !sc
      | L.null $ _stack sc = (False, sc)
      | L.null $ _rest sc = (False, sc)
      | otherwise = case step sc of
                      r@(True, _) -> r
                      (False, sc') -> loop sc' -- trace (show sc') sc'
      where
        step sc = 
          let (t, sn):rest = _rest sc
          in probe t sn sc{_rest = rest}
    
        probe t sn sc
          | isTagOpen t =
            let (found, mcs, ccmap) = moving critv t sn sc
                stack = (t, sn, 1 + _nc sc) : _stack sc
            in --trace (show found ++ ", " ++ show stack ++ "\n" ++ show mcs)
              (found, sc{_stack = stack, _nc = 0, _mcs = mcs, _ccmap = ccmap})
      
          | isTagClose t = 
            let (t1, sn1, nc1):stack = _stack sc
                nc = _nc sc
                ccmap = IntMap.insert sn1 nc $ _ccmap sc
                mcs = moveout $ _mcs sc
            in (False, sc{_ccmap = ccmap, _nc = nc1, _stack = stack, _mcs = mcs})
    
          | otherwise = (False, sc)

{-# INLINE moving #-}
moving :: V.Vector Criterion -> Textag -> Int -> Context -> (Bool, [MatchCandi], IntMap Int)
moving critv t sn sc
  | V.length critv == 0 = (True, [], _ccmap sc)
  | otherwise = case extPath (_mcs sc) of
                  (True, b, mcs) -> (b, mcs, _ccmap sc)
                  (False, _, _) ->
                    let ccmap = countChild (_stack sc) ((t, sn):_rest sc) (_nc sc) (_ccmap sc)
                    in  moving critv t sn sc{_ccmap = ccmap}
  where
    extPath mcs = let i = (1 + _nc sc)
                      (_, psn, _):_ = _stack sc
                  in L.foldr (eachPath i psn) (True, False, []) $ [-1]:mcs
    eachPath _ _ _ (False, _,  _) = (False, False, [])
    eachPath i psn mc@(c:_) (True, b, r) =
      case critMatch t sn i psn (_ccmap sc) (critv V.! (c + 1)) of
        Just True ->  let found = c == V.length critv - 2
                      in (True, found,
                           case (found, c) of
                             (True, -1) -> r
                             (True, _) -> (c:mc):r
                             (False, -1) -> [0]:r
                             (False, _) -> ((c +1):mc):(c:mc):r
                         )
        Just False -> (True, b,
                        if c == -1 then r else (c:mc):r)
        Nothing -> (False, False, [])
  
{-# INLINE moveout #-}
moveout :: [MatchCandi] -> [MatchCandi]
moveout = L.foldr (\cp r -> case cp of [_] -> r; _:tx -> tx:r) []

countChild :: TagStack -> TagList -> Int -> IntMap Int -> IntMap Int
countChild (s1@(tag, sn, i):_) rest nc countMap =
  loop [(tag, sn, i)] rest nc countMap
  where
    loop [] _ _ cm = cm
    loop _ [] _ _ = error "childCount: empty rest tags, tags are not enough"
    loop (s:sx) ((tag, sn):ax) nc cm
      |isTagOpen tag = loop ((tag, sn, nc + 1):s:sx) ax 0 cm

      |isTagClose tag, (tag1, sn1, n1) <- s =
         loop sx ax n1 $ IntMap.insert sn1 nc cm

      |otherwise = loop (s:sx) ax nc cm

restrict :: Select ()
restrict = Select $ \sc ->
  let stack  = case  _stack sc of
                 s:_ -> [s]
                 [] -> []
  in  ((), sc{_stack = stack, _mcs = []})

stay :: Select a -> Select a
stay p = do
  sc <- context
  x <- p
  cmodify $ const sc
  return x

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

critMatch :: Textag -> -- ^ Tag to be compare
             Int ->    -- ^ Serial Number of this tag
             Int ->    -- ^ Order number in its parent
             Int ->    -- ^ Serial Number of its parent tag
             IntMap Int -> -- ^ Child tag count map
             Criterion ->  -- ^ Condition express
             Maybe Bool    -- ^ Just bool is result, nothing means its parent count not in map
critMatch t sn i psn ccm (Criterion entCrit adCrit) =
  if tagMatch t entCrit
  then adMatch adCrit i
  else Just False
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

    adMatch :: AdCrit -> Int -> Maybe Bool
    adMatch AdNone _ = Just True
    adMatch FirstChild i = Just $! i == 1
    adMatch LastChild _ = requireParentcc psn i
    adMatch (NthChild n) i = Just $! i == n
    adMatch (NthLastChild n) i = requireParentcc psn (n + i - 1)
    
    requireParentcc sn i = do
      cc <- IntMap.lookup sn ccm
      return $ i == cc

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
      | ads == ":last-child" = LastChild
      | Just s <- T.stripPrefix ":nth-child(" ads =  case T.decimal s of
                                                       Right (i, ")") -> NthChild i
                                                       other -> wrong $ show other
      | Just s <- T.stripPrefix ":nth-last-child(" ads =  case T.decimal s of
                                                            Right (i, ")") -> NthLastChild i
                                                            other -> wrong $ show other
      | otherwise = wrong $ T.unpack ads
    wrong reason = error $ "unrecongnized tag selector " ++ reason

first :: (a, b, c) -> a
first (a, _, _) = a

second :: (a, b, c) -> b
second (_, b, _) = b

third :: (a, b, c) -> c
third (_, _, c) = c
