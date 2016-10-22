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
  childCount
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
  let (found, sc1) = loop False sc0
      sc2 =  if found then sc1 else sc0{_ccmap = _ccmap sc1}
  in (found, sc2)
  where
    loop countOnly sc
      | L.null $ _stack sc = (False, sc)
      | L.null $ _rest sc = (False, sc)
      | otherwise = case step countOnly sc of
                      r@(True, _) -> r
                      (False, sc') -> loop countOnly sc' -- trace (show sc') sc'
  
    step countOnly sc = 
      let (t, sn):rest = _rest sc
      in probe countOnly t sn sc{_rest = rest}
 
    probe countOnly t sn sc
      | isTagOpen t =
          let (found, mcs, ccmap) = if countOnly
                                    then (False, [], _ccmap sc)
                                    else moving t sn sc
              stack = (t, sn, 1 + _nc sc) : _stack sc
          in --trace (show found ++ ", " ++ show stack ++ "\n" ++ show mcs)
            (found, sc{_stack = stack, _nc = 0, _mcs = mcs, _ccmap = ccmap})
 
      | isTagClose t = 
          let (t1, sn1, nc1):stack = _stack sc
              nc = _nc sc
              ccmap = IntMap.insert sn1 nc $ _ccmap sc
              mcs = if countOnly then [] else moveout $ _mcs sc
          in (False, sc{_ccmap = ccmap, _nc = nc1, _stack = stack, _mcs = mcs})

      | otherwise = (False, sc)

    moving :: Textag -> Int -> Context -> (Bool, [MatchCandi], IntMap Int)
    moving t sn sc
      | V.length critv == 0 = (True, [], _ccmap sc)
      | otherwise = case extPath (_mcs sc) of
                      (True, b, mcs) -> (b, mcs, _ccmap sc)
                      (False, _, _) -> let s0:_ = _stack sc
                                           rest = (t,sn):_rest sc
                                           (False, sc') = loop True sc{_stack = [s0], _rest = rest}
                                       in  moving t sn sc{_ccmap = _ccmap sc'}
      where
        extPath mcs = let i = (1 + _nc sc)
                          (_, psn, _) = head $ _stack sc
                      in L.foldr (eachPath i psn) (True, False, []) $ [-1]:mcs
        eachPath _ _ _ (False, _,  _) = (False, False, [])
        eachPath i psn mc@(c:_) (True, b, r) =
          case critMatch' t sn i psn (_ccmap sc) (critv V.! (c + 1)) of
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

    moveout :: [MatchCandi] -> [MatchCandi]
    moveout = L.foldr (\cp r -> case cp of [_] -> r; _:tx -> tx:r) []


unless' :: Select Bool -> Select Bool -> Select Bool
unless' p q = do
  b <- p
  if b
    then return False
    else q

when' :: Select Bool -> Select Bool -> Select Bool
when' p q = do
  b <- p
  if b
    then q
    else return False

seek :: [Criterion] -> Context -> (Bool, Context)
seek critx (Context stack rest nc mc ccmap) =
  -- trace (show stack ++ ", " ++ show (take 5 rest)) $
  (found, Context stack' rest' nc' mc ccmap')
  where
    (found, stack', nc', rest', ccmap') = search stack nc rest ccmap

    search [] _ _ ccm = (False, stack, nc, rest, ccm) -- stack empty, means not found within current sub tree
    search _ _ [] ccm = (False, stack, nc, rest, ccm) -- no more nodes
    search !stack i ((a, sn):ax) ccm
      | isTagOpen a, stack' <- (a, sn, i + 1):stack =
          case  pathMatch critx stack' of
            True -> (True, stack', 0, ax, ccm)
            False -> search stack' 0  ax ccm

      | isTagClose a, (tag1, sn1, nc1):sx <- stack =
          search sx nc1 ax $ IntMap.insert sn1 i ccm

      | otherwise = search stack i ax ccm

pathMatch :: [Criterion] -> [(Textag, Int, Int)] -> Bool
pathMatch [] _ = True -- empty conditon means match any node
pathMatch (c:cx) ((a, _, i):ax)
  | critMatch a i c = lookupper cx ax
  | otherwise =  False
  where
    lookupper [] _ = True  -- all condition matchs
    lookupper _ [] = False
    lookupper (c:cx) !ax  = case dropWhile (\(a, _, i) -> not $! critMatch a i c) ax of
                              [] -> False
                              _:rest -> case lookupper cx rest of
                                          True -> True
                                          False -> lookupper (c:cx) rest

-- probe :: Vector Criterion -> [CandidatePath] -> Textag -> Int -> (Bool, [CandidatePath])
-- probe ctx cpx t i = loop cpx False []
--   where
--     loop [] b rx = if match t i (ctx // 0)
--                    then (b || V.length ctx == 1, [0]:rx)
--                    else (b, rx)
--     loop (cp:cpx) b rx | c:_ <-  cp = let bnext = c < V.length - 1 && match t i (ctx // (c + 1))
--                                           rx' =  case bnext of
--                                             True -> ((c+1):cp):(c:cp):rx
--                                             False -> (c:cp):rx
--                                        in loop cpx (b || c + 2 == V.length) rx'

-- pathMatch2 :: [Criterion] -> [(Textag, Int, Int)] -> Either [[(Int, Int -> Bool)]] Bool
-- pathMatch2 _ []  = True
-- pathMatch2 (b:bx) ((a, sn, i):ax) = case critMatch a i b of
--                                      CritMath -> lookupper bx ax []
--                                      NeedParentCc p -> lookupper bx ax [(sn, p)]
--                                      CritNotMatch -> Right False
--   where
--     lookupper [] _ [] = Right True
--     lookupper [] _ px = Left px
--     lookupper _ [] _ = Right False
--     lookupper (b:bx) !ax  px = case loop b ax of
--       Right (True, rest) -> lookupper bx rest px
--       Right (False, _) -> Right False
--       Left [(p, rest)] -> lookupper bx rest (p:px)

--     loop _ [] px = (Right False, [])
--     loop c ((a,sn,i):ax) = case critMatch' a i c of
--                            CritMath -> (Right True, ax)
--                            NeedParentCc p -> (Left (sn, p), ax)
--                            CritNotMatch -> m1 c ax

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
    adMatch LastChild _ = True  -- outter function handle this
    adMatch (NthChild n) i = i == n


critMatch' :: Textag -> -- ^ Tag to be compare
              Int ->    -- ^ Serial Number of this tag
              Int ->    -- ^ Order number in its parent
              Int ->    -- ^ Serial Number of its parent tag
              IntMap Int -> -- ^ Child tag count map
              Criterion ->  -- ^ Condition express
              Maybe Bool    -- ^ Just bool is result, nothing means its parent count not in map
critMatch' t sn i psn ccm (Criterion entCrit adCrit) =
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


childCount :: [(Textag, Int, Int)] -> [(Textag, Int)] -> Int -> IntMap Int -> (Int, IntMap Int)
childCount (s1@(tag, sn, i):_) rest nc countMap = -- nc means visited child node count of top stack tag
  case IntMap.lookup sn countMap of
    Just v -> (v, countMap)
    Nothing -> loop [(tag, sn, i)] rest nc countMap
  where
    loop [] _ _ cm = case IntMap.lookup sn cm of
                           Just v -> (v, cm)
                           Nothing -> error $ "childCount: root of subtree not in map, " ++ show (tag, sn, i)

    loop _ [] _ _ = error "childCount: empty rest tags, tags are not enough"

    loop (s:sx) ((tag, sn):ax) nc cm
      |isTagOpen tag = loop ((tag, sn, nc + 1):s:sx) ax 0 cm

      |isTagClose tag, (tag1, sn1, n1) <- s =
         loop sx ax n1 $ IntMap.insert sn1 nc cm

      |otherwise = loop (s:sx) ax nc cm

-- type Stack sa = [(Textag, Int, (Int, sa))]
-- type TagList ta = [(Textag, Int, ta)]

-- travel :: Stack sa -> TagList ta -> Int -> st -> (Stack sa, TagList ta, Int, st)
-- travel stack rest nc st = loop stack rest nc st
--   where
--     loop [] rest nc st = ([], rest, nc, st)
--     loop stack [] nc st = (stack, [], nc, st)
--     loop (s:sx) ((tag, sa):ax) nc st
--       | isTagOpen tag = loop
--       | otherwise = let (tag, st):ax rest
--                     in isTagOpen


first :: (a, b, c) -> a
first (a, _, _) = a

second :: (a, b, c) -> b
second (_, b, _) = b

third :: (a, b, c) -> c
third (_, _, c) = c
