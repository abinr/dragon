module Main

import System

readNumber : String -> IO (Maybe Nat)
readNumber input = do
  if all isDigit (unpack input)
  then pure (Just (cast input))
  else pure Nothing

guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target guesses = do
  putStrLn ("Number of attempts: " ++ show guesses ++ ".")
  putStr ("Guess a number between 1 and 100: ")
  input <- getLine
  n <- readNumber input
  case n of
    Nothing => do
      putStrLn "Invalid input. Try again."
      guess target (guesses + 1)
    Just x =>
      case compare x target of
        LT => do
          putStrLn "Too Low."
          guess target (guesses + 1)
        EQ =>
          putStrLn "Correct!"
        GT => do
          putStrLn "Too High."
          guess target (guesses + 1)

main : IO ()
main = do
  time <- System.time
  guess (cast (mod time 100)) 0
