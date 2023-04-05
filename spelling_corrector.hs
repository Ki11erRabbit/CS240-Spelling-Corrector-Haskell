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

suggest_similar_word :: String -> IO SpellingCorrector -> Maybe String
suggest_similar_word input corrector = do
  let input_lower = map toLower input
  case find_string input_lower (dictionary corrector) of
    Just trie -> Just input_lower
    Nothing ->  Nothing-- do
      --let edit_dist1 = gen_edit_dist1 input_lower



gen_edit_dist1 :: String -> HashSet.HashSet String
gen_edit_dist1 word = HashSet.union (insert_char word) (HashSet.union (alternate_char word) (HashSet.union (delete_char word) (transpose_char word)))

delete_char :: String -> HashSet.HashSet String
delete_char str = HashSet.fromList (delete_char_helper str 0)

delete_char_helper :: String -> Int -> [String]
delete_char_helper str pos = if length str == pos then [] else ((take pos str) ++ (drop (pos + 1) str)):delete_char_helper str (pos + 1)


transpose_char :: String -> HashSet.HashSet String
transpose_char str = HashSet.fromList (transpose_char_helper str 0 1)

transpose_char_helper :: String -> Int -> Int -> [String]
transpose_char_helper str pos1 pos2 = if length str == pos1 then [] else "todo":transpose_char_helper str (pos1 +1) (pos2 +1)

alternate_char :: String -> HashSet.HashSet String
alternate_char str = HashSet.fromList ["todo", "not","implemented"]

insert_char :: String -> HashSet.HashSet String
insert_char str = HashSet.fromList ["todo", "not","implemented"]
