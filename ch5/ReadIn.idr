module ReadIn

import Data.Vect

readToBlank : IO (List String)
readToBlank = do
  line <- getLine
  if (line == "")
  then 
    pure []
  else do
    list <- readToBlank
    pure (line :: list)

readAndSave : IO ()
readAndSave = do
  input <- readToBlank
  putStr "Save to file: "
  file <- getLine
  Right f <- writeFile file (concat input)
    | Left err => putStrLn (show err)
  pure ()

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
  Right f <- readFile filename
    | Left err => pure (_ ** [])
  let lines = lines f
  pure (_ ** fromList lines)
