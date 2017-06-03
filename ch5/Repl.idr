module Main

io : (prompt : String) -> (onInput : String -> String) -> IO ()
io prompt onInput = do
  putStr prompt
  line <- getLine
  putStr (onInput line)
  io prompt onInput

ioWith : (state : a) 
  -> (prompt: String) 
  -> (onInput: a 
  -> String 
  -> Maybe (String, a)) 
  -> IO ()
ioWith state prompt onInput = do
  putStr prompt
  line <- getLine
  let result = (onInput state line)
  case result of
    Nothing => do 
      putStrLn "Goodbye."
    (Just (str, s)) => do 
      putStrLn str
      ioWith s prompt onInput
