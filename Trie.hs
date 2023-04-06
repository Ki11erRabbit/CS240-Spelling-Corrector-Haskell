{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Trie where

import Data.List
import Data.Char (ord)
import Data.Maybe
import Data.Ord
import qualified Data.Maybe as Maybe

import Debug.Trace

data Trie = Trie {
  value :: Maybe Char,
  freq :: Int,
  children :: [Maybe Trie] 
  } 

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


instance Ord Trie where
  compare right left = compare (freq right) (freq left)

instance Eq Trie where
  (==) trieL trieR = if children trieL /= children trieR then False
    else if value trieL /= value trieR then False
    else if freq trieL /= freq trieR then False
    else True

  (/=) right left = not (right == left)


instance Show Trie where
  show trie = concatMap (show_helper "") (children trie)

show_helper :: String -> Maybe Trie -> String
show_helper str Nothing = ""
show_helper str (Just trie) = if freq trie >= 1 then holder ++ '\n':concatMap (show_helper holder) (children trie) else concatMap (show_helper holder) (children trie) 
  where holder = str ++ [maybe '\t' id (value trie)]


get_node_count :: Trie -> Int
get_node_count trie = sum (map get_node_count_helper (children trie))

get_node_count_helper :: Maybe Trie -> Int
get_node_count_helper Nothing = 0
get_node_count_helper (Just trie) = 1 + get_node_count trie

get_word_count :: Trie -> Int
get_word_count trie = sum (map get_word_count_helper (children trie))

get_word_count_helper :: Maybe Trie -> Int
get_word_count_helper Nothing = 0
get_word_count_helper (Just trie) = freq trie + get_word_count trie
