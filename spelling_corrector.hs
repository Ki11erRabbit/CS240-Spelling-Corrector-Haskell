{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Trie

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO (withFile, IOMode(ReadMode), hGetContents, Handle)
import Data.List
import Data.Char (toLower)
import qualified Data.HashSet as HashSet
import Data.Ord
import Data.Maybe

data SpellingCorrector = SpellingCorrector {
  dictionary :: Trie
  } deriving (Show)

new_corrector :: SpellingCorrector
new_corrector = SpellingCorrector (make_trie Nothing)

file_to_list :: FilePath -> IO ()
file_to_list file_path = readFile file_path >>= print


load_dictionary :: FilePath -> SpellingCorrector -> IO SpellingCorrector
load_dictionary file_name corrector = do
  string <- readFile file_name
  return $ SpellingCorrector { dictionary = load_dictionary_helper (words string) (dictionary corrector)}

                                     
load_dictionary_helper :: [String] -> Trie -> Trie
load_dictionary_helper file_name dict = foldl (\dict word -> add_string word dict) dict file_name

suggest_similar_word :: String -> SpellingCorrector -> Maybe String
suggest_similar_word input corrector = do
  let input_lower = map toLower input
  case find_string input_lower (dictionary corrector) of
    Just trie -> Just input_lower
    Nothing ->  Nothing-- do
      --let edit_dist1 = gen_edit_dist1 input_lower

testFunc :: String -> String -> IO ()
testFunc file_name input = do
  corrector <- load_dictionary file_name new_corrector
  case suggest_similar_word input corrector of
    Just word -> putStrLn word
    Nothing -> do
      let edit_dist1 = gen_edit_dist1 input
      case HashSet.size (find_in_edit_dist edit_dist1 (dictionary corrector)) of
        0 ->do
          let edit_dist2 = gen_edit_dist2 (HashSet.toList edit_dist1)
          case HashSet.size (find_in_edit_dist edit_dist2 (dictionary corrector)) of
            0 -> putStrLn "No suggestions found"
            _ -> do
              let words = HashSet.toList (find_in_edit_dist edit_dist2 (dictionary corrector))
              let zipped_nodes = zip words (map (\word -> find_string word (dictionary corrector)) words)
              let max_node = maximum zipped_nodes
              let word = fst max_node
              putStrLn word
        _ -> do
          let words = HashSet.toList (find_in_edit_dist (gen_edit_dist1 input) (dictionary corrector))
          let zipped_nodes = zip words (map (\word -> find_string word (dictionary corrector)) words)
          let max_node = maximum zipped_nodes
          let word = fst max_node
          putStrLn word
      

find_in_edit_dist :: HashSet.HashSet String -> Trie -> HashSet.HashSet String
find_in_edit_dist edit_dist dict = HashSet.filter (\word -> isJust (find_string word dict)) edit_dist

gen_edit_dist1 :: String -> HashSet.HashSet String
gen_edit_dist1 word = HashSet.union (insert_char word) (HashSet.union (alternate_char word) (HashSet.union (delete_char word) (transpose_char word)))

gen_edit_dist2 :: [String] -> HashSet.HashSet String
gen_edit_dist2 [] = HashSet.empty
gen_edit_dist2 words = HashSet.fromList (concatMap (\word -> HashSet.toList (gen_edit_dist1 word)) words)

delete_char :: String -> HashSet.HashSet String
delete_char str = HashSet.fromList (delete_char_helper str 0)

delete_char_helper :: String -> Int -> [String]
delete_char_helper str pos = if length str == pos then []
  else (take pos str ++ drop (pos + 1) str):delete_char_helper str (pos + 1)


transpose_char :: String -> HashSet.HashSet String
transpose_char str = HashSet.fromList (trans_helperI 0 str)


trans_helperI :: Int -> String -> [String]
trans_helperI i str = if length str == i then []
  else trans_helperJ i (i +1) str ++ trans_helperI (i +1) str

trans_helperJ :: Int -> Int -> String -> [String]
trans_helperJ i j str = if length str == j then []
  else replaceChar (str!!j) i (replaceChar (str!!i) j str):trans_helperJ i (j +1) str
  
replaceChar :: Char -> Int -> String -> String
replaceChar c pos str = take pos str ++ (c : drop (pos+1) str)
            
alternate_char :: String -> HashSet.HashSet String
alternate_char str = HashSet.fromList (alternate_char_helper str alphabet 0)
  where alphabet = "abcdefghijklmnopqrstuvwxyz"

type Alphabet = String

alternate_char_helper :: String -> Alphabet -> Int -> [String]
alternate_char_helper str alphabet pos = if length str == pos then []
  else alt_char_helper pos 0 str alphabet ++ alternate_char_helper str alphabet (pos + 1)

alt_char_helper :: Int -> Int -> String -> Alphabet -> [String]
alt_char_helper pos index str alphabet = if length alphabet == index then []
  else replaceChar (alphabet!!index) pos str:alt_char_helper pos (index + 1) str alphabet

insert_char :: String -> HashSet.HashSet String
insert_char str = HashSet.fromList (insert_char_helper str alphabet 0)
  where alphabet = "abcdefghijklmnopqrstuvwxyz"

insert_char_helper :: String -> Alphabet -> Int -> [String]
insert_char_helper str alphabet pos = if length str == pos then insert_helper_end str alphabet
  else insert_helper str alphabet pos 0 ++ insert_char_helper str alphabet (pos + 1)

insert_helper :: String -> Alphabet -> Int -> Int -> [String]
insert_helper str alphabet pos index = if length alphabet == index then []
  else (take pos str ++ alphabet!!index : drop pos str):insert_helper str alphabet pos (index + 1)

insert_helper_end :: String -> Alphabet -> [String]
insert_helper_end str [] = []
insert_helper_end str (x:xs) = (str ++ [x]):insert_helper_end str xs
