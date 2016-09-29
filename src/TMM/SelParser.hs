-- -*- coding: utf-8 -*-
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module TMM.SelParser
where

import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Map as M
import qualified Text.ParserCombinators.Parsec((<|>))
import qualified Text.ParserCombinators.Parsec as P
import qualified Text.ParserCombinators.Parsec.Expr as E
import qualified Text.ParserCombinators.Parsec.Pos as Pos
import qualified Text.Parsec.Prim as Pr
import Control.Applicative
import Control.Monad.Identity (Identity)
import Debug.Trace

-- alias Parsec.parse for more concise usage in my examples:
parse rule text = P.parse rule "(source)" text


data Regexy = StartsWith
            |   EndsWith
            |   Contains
            |   Equals
            |   Anything
            |   ContainsWord
            |   NotEquals
            deriving (Eq, Show)

data CssSel =  Any
              |  Type Text
              |  Id Text
              |  Class Text
              |  Attribute  Regexy Text Text
              |  And [CssSel]
              |  Or  [CssSel]
              |  Not [CssSel]
              |  Has [CssSel]
              |  ParentIs CssSel
              |  AncestorIs CssSel
              -- Operators
              |  Descendant
              |  DirectChild
              |  NextAdjecent
              |  NextSibling
              deriving (Eq, Show)

uline :: P.Parser Char
uline = P.char '_'

indetifier :: P.Parser Text
indetifier = do
  first <- P.letter <|> uline
  rest <- P.many (P.letter <|> P.digit <|> uline )
  return $ T.pack $ first:rest
  
parseClassName :: P.Parser CssSel
parseClassName = do
  dot <- P.char '.'
  name <- indetifier
  return $ Class name

parseIdent :: P.Parser CssSel
parseIdent = do
  sharp <- P.char '#'
  name <- indetifier
  return $ Id name

parseType :: P.Parser CssSel
parseType = do
  name <- indetifier
  return $ Type name

parseDescendant :: P.Parser (Maybe CssSel)
parseDescendant = do
  P.space
  P.spaces
  return $Just Descendant

parseRelation :: P.Parser (Maybe CssSel)
parseRelation = do
  P.spaces
  direct <|> adjecent <|> sibling
  where
    direct = do
      P.char '>'
      return $ Just DirectChild
    adjecent = do
      P.char '+'
      return $ Just NextAdjecent
    sibling = do
      P.char '~'
      return $ Just NextSibling
    

ended :: P.Parser (Maybe CssSel)
ended = do
  P.spaces
  P.eof
  return Nothing

-- parseSep :: P.Parser (Maybe CssSel)
-- parseSep = do
--   direct_ <- P.optionMaybe $ P.try parseDirectChild
--   r <- case direct_ of
--          Nothing -> do
--            e <- P.optionMaybe $ P.try ended
--            case e of
--              Just _ -> return Nothing
--              Nothing ->  return $ Just Descendant
--          Just d -> return $ Just d
--   P.spaces
--   return r
  
parseCssSels :: P.Parser [CssSel]
parseCssSels =  loop
  where
    loop = do
      P.spaces
      ent <- parseClassName <|> parseIdent <|> parseType
      rx <- remains
      return $ ent:rx

    remains = do
      sep <- P.try parseRelation <|> P.try ended <|> parseDescendant
      case sep of
        Nothing -> return []
        Just s -> do
          rx <- loop
          return $ s:rx


          

    


  

  
