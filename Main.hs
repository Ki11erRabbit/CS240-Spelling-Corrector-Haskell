import SpellingCorrector

import System.Environment
import Data.List
import Trie (add_string, get_node_count, get_word_count)


main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> interactiveMode
    1 -> if head args == "--help" then
      putStrLn "Usage: spelling-corrector [dictionary file] [word to check]"
      else do
      let dictFile = head args
      putStrLn "Reading dictionary..."
      let spcorrector = new_corrector
      corrector <- load_dictionary dictFile spcorrector
      putStrLn "Dictionary loaded"
      mainLoop corrector
    2 -> do
      let dictFile = head args
      let word = args !! 1
      putStrLn "Reading dictionary..."
      let spcorrector = new_corrector
      corrector <- load_dictionary dictFile spcorrector
      putStrLn "Dictionary loaded"
      let suggestion = suggest_similar_word word corrector
      case suggestion of 
        Nothing -> putStrLn "No suggestions found"
        Just suggestion -> putStrLn $ "Suggestion: " ++ suggestion


interactiveMode :: IO ()
interactiveMode = do
  putStrLn "Enter a dictionary file: "
  dictFile <- getLine
  putStrLn "Reading dictionary..."
  let spcorrector = new_corrector
  corrector <- load_dictionary dictFile spcorrector
  putStrLn "Dictionary loaded"
  mainLoop corrector

mainLoop :: SpellingCorrector -> IO ()
mainLoop corrector = do
  putStrLn "Enter a word to check: (:o to see options)"
  word <- getLine
  case word of
    ":q" -> return ()
    (':':'l':' ':word) -> do
      n_corrector <- load_dictionary word corrector
      mainLoop n_corrector
    ":o" -> do
      putStrLn "Options:\n :q - quit\n :l [word] - add word to dictionary\n :o - show options\n :r - reload dictionary\n :s - show dictionary\n :n - show number of nodes in dictionary\n :w - show number of words in dictionary"
      mainLoop corrector
    (':':'r':' ':dict_name) -> do
      n_corrector <- load_dictionary dict_name corrector
      mainLoop n_corrector
    ":s" -> do
      putStrLn $ "Dictionary: \n" ++ (show $ dictionary corrector)
      mainLoop corrector
    ":n" -> do
      putStrLn $ "Number of nodes in dictionary: " ++ (show $ get_node_count (dictionary corrector))
      mainLoop corrector
    ":w" -> do
      putStrLn $ "Number of words in dictionary: " ++ (show $ get_word_count (dictionary corrector))
      mainLoop corrector
    _ -> do
      let suggestion = suggest_similar_word word corrector
      case suggestion of 
        Nothing -> putStrLn "No suggestions found"
        Just suggestion -> putStrLn $ "Suggestion: " ++ suggestion
      mainLoop corrector


