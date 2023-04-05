import Trie

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO (withFile, IOMode(ReadMode), hGetContents, Handle)
import Data.List
import Data.Char (toLower)
import qualified Data.HashSet as HashSet

data SpellingCorrector = SpellingCorrector {
  dictionary :: Trie
  } deriving (Show)

new_corrector :: SpellingCorrector
new_corrector = SpellingCorrector (make_trie Nothing)

--getlines :: Handle -> IO [String]
--getlines h = Data.List.lines `fmap` hGetContents h

{-makeLower :: [String] -> [String]
makeLower [] = []
makeLower input = Data.List.map Text.unpack (Data.List.map toLower (Data.List.map Text.pack input))-}

{-file_to_list :: FilePath -> IO [String]
file_to_list file_path = withFile file_path ReadMode $ \handle -> do
  contents <- hGetContents handle
  return $ map (map toLower) (lines contents) -}

file_to_list :: FilePath -> IO ()
file_to_list file_path = readFile file_path >>= print

{-file_to_list :: FilePath -> IO [String]-}

load_dictionary :: FilePath -> SpellingCorrector -> IO SpellingCorrector
load_dictionary file_name corrector = do
  string <- readFile file_name
  return $ SpellingCorrector { dictionary = load_dictionary_helper (words string) (dictionary corrector)}

{-load_dictionary :: String -> SpellingCorrector -> SpellingCorrector
load_dictionary file_name corrector = do
  list <- file_to_list file_name
  SpellingCorrector { dictionary = load_dictionary_helper list (dictionary corrector)}-}
  --corrector { dictionary = foldl (\dict word -> add_string word dict) (dictionary corrector) list}
                                     
load_dictionary_helper :: [String] -> Trie -> Trie
load_dictionary_helper [] dict = dict
load_dictionary_helper (x:xs) dict = load_dictionary_helper xs (add_string x dict)

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
    Nothing -> putStrLn "No suggestions"

  

gen_edit_dist1 :: String -> HashSet.HashSet String
gen_edit_dist1 word = HashSet.union (insert_char word) (HashSet.union (alternate_char word) (HashSet.union (delete_char word) (transpose_char word)))

delete_char :: String -> HashSet.HashSet String
delete_char str = HashSet.fromList (delete_char_helper str 0)

delete_char_helper :: String -> Int -> [String]
delete_char_helper str pos = if length str == pos then [] else ((take pos str) ++ (drop (pos + 1) str)):delete_char_helper str (pos + 1)


transpose_char :: String -> HashSet.HashSet String
transpose_char str = HashSet.fromList (trans_helperI 0 str)


trans_helperI :: Int -> String -> [String]
trans_helperI i str = if length str == i then [] else (trans_helperJ i (i +1) str) ++ trans_helperI (i +1) str

trans_helperJ :: Int -> Int -> String -> [String]
trans_helperJ i j str = if length str == j then [] else (replaceChar (str!!j) i (replaceChar (str!!i) j str)):trans_helperJ i (j +1) str
  
replaceChar :: Char -> Int -> String -> String
replaceChar c pos str = (take pos str) ++ (c : (drop (pos+1) str))
            
alternate_char :: String -> HashSet.HashSet String
alternate_char str = HashSet.fromList (alternate_char_helper str alphabet 0)
  where alphabet = "abcdefghijklmnopqrstuvwxyz"

type Alphabet = String

alternate_char_helper :: String -> Alphabet -> Int -> [String]
alternate_char_helper str alphabet pos = if length str == pos then [] else (alt_char_helper pos 0 str alphabet) ++ alternate_char_helper str alphabet (pos + 1)

alt_char_helper :: Int -> Int -> String -> Alphabet -> [String]
alt_char_helper pos index str alphabet = if length alphabet == index then [] else (replaceChar (alphabet!!index) pos str):alt_char_helper pos (index + 1) str alphabet

insert_char :: String -> HashSet.HashSet String
insert_char str = HashSet.fromList (insert_char_helper str alphabet 0)
  where alphabet = "abcdefghijklmnopqrstuvwxyz"

insert_char_helper :: String -> Alphabet -> Int -> [String]
insert_char_helper str alphabet pos = if length str == pos then insert_helper_end str alphabet else (insert_helper str alphabet pos 0) ++ insert_char_helper str alphabet (pos + 1)

insert_helper :: String -> Alphabet -> Int -> Int -> [String]
insert_helper str alphabet pos index = if length alphabet == index then [] else (take pos str ++ alphabet!!index : (drop pos str)):insert_helper str alphabet pos (index + 1)

insert_helper_end :: String -> Alphabet -> [String]
insert_helper_end str [] = []
insert_helper_end str (x:xs) = (str ++ [x]):insert_helper_end str xs
