import Trie

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO (withFile, IOMode(ReadMode), hGetContents, Handle)
import Data.List
import Data.Char (toLower)

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
  return $ SpellingCorrector { dictionary = load_dictionary_helper (lines string) (dictionary corrector)}

{-load_dictionary :: String -> SpellingCorrector -> SpellingCorrector
load_dictionary file_name corrector = do
  list <- file_to_list file_name
  SpellingCorrector { dictionary = load_dictionary_helper list (dictionary corrector)}-}
  --corrector { dictionary = foldl (\dict word -> add_string word dict) (dictionary corrector) list}
                                     
load_dictionary_helper :: [String] -> Trie -> Trie
load_dictionary_helper [] dict = dict
load_dictionary_helper (x:xs) dict = load_dictionary_helper xs (add_string x dict)
                                     
                                     
