import SpellingCorrector

import System.Environment
import Data.List
import Trie (add_string)


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
  putStrLn "Enter a word to check: (:q to quit)"
  word <- getLine
  case word of
    ":q" -> return ()
    (':':'l':' ':word) -> do
      let n_corrector' = SpellingCorrector (add_string word (dictionary corrector))
      mainLoop n_corrector'
    ":o" -> do
      putStrLn "Options:\n :q - quit\n :l [word] - add word to dictionary\n :o - show options\n :r - reload dictionary\n :s - show dictionary"
      mainLoop corrector
    (':':'r':' ':dict_name) -> do
      n_corrector <- load_dictionary dict_name corrector
      mainLoop n_corrector
    ":s" -> do
      putStrLn $ "Dictionary: " ++ (show $ dictionary corrector)
      mainLoop corrector
    _ -> do
      let suggestion = suggest_similar_word word corrector
      case suggestion of 
        Nothing -> putStrLn "No suggestions found"
        Just suggestion -> putStrLn $ "Suggestion: " ++ suggestion
      mainLoop corrector


