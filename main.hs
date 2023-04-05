import SpellingCorrector



main :: IO ()
main = do
  putStrLn "Enter a dictionary file: "
  dictFile <- getLine
  putStrLn "Reading dictionary...\nThis may take a while..."
  let spcorrector = new_corrector
  corrector <- load_dictionary dictFile spcorrector
  putStrLn "Dictionary loaded"
  mainLoop corrector



mainLoop :: SpellingCorrector -> IO ()
mainLoop corrector = do
  putStrLn "Enter a word to check: (:q to quit)"
  word <- getLine
  if word == ":q"
    then return ()
    else do
      let suggestion = suggest_similar_word word corrector
      case suggestion of 
        Nothing -> putStrLn "No suggestions found"
        Just suggestion -> putStrLn $ "Suggestion: " ++ suggestion
      mainLoop corrector
