module Trie where

import Data.List
import Data.Char (ord)
import Data.Maybe

import Debug.Trace

data Trie = Trie {
  value :: Maybe Char,
  freq :: Int,
  children :: [Maybe Trie] 
  } deriving (Show)

char_to_index :: Char -> Int
char_to_index c = (ord c) - (ord 'a')

insert_at_pos :: Int -> Maybe Trie -> [Maybe Trie] -> [Maybe Trie]
insert_at_pos pos val list = (take pos list) ++ (val : (drop (pos+1) list))

make_trie :: Maybe Char -> Trie
make_trie char = Trie char 0 (replicate 26 Nothing)

increment_trie :: Trie -> Trie
increment_trie trie = trie { freq = freq trie + 1}

add_string :: String -> Trie -> Trie
add_string [] trie = increment_trie trie
add_string (c:str) trie = let ct = children trie
                              in case ct!!(char_to_index c) of
                                   Nothing -> trie { children = (insert_at_pos (char_to_index c) (Just (add_string str (make_trie (Just c)))) ct) }
                                   Just x -> trie { children = (insert_at_pos (char_to_index c) (Just (add_string str x)) ct) }


find_string :: String -> Trie -> Maybe Trie
find_string [] trie = if freq trie >= 1 then Just trie else Nothing
find_string (c:str) trie = let child_tries = children trie
                               in case child_tries!!(char_to_index c) of
                                    Nothing -> Nothing
                                    Just child_trie -> find_string str child_trie


{-class ShowHelper a where
  show_helper :: a -> String

instance ShowHelper Trie where
  show_helper trie = concatMap (
    \ltrie -> case value ltrie of
      Nothing -> ""
      Just ctrie -> (maybe '\t' id value ctrie : show_helper ltrie) ++ if freq trie >= 1 then "\n" else "") children trie

instance ShowHelper (Maybe Trie) where
  show_helper Nothing = ""
  show_helper Just trie = concatMap (
    \ltrie -> case value ltrie of
      Nothing -> ""
      Just ctrie -> (maybe '\t' id value ctrie : show_helper ltrie) ++ if freq trie >= 1 then "\n" else "") children trie

instance Show Trie where
  show trie = show_helper trie -}
