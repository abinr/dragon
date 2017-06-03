printLength : IO ()
printLength = putStr "Input String: " >>= \_ =>
              getLine >>= \input =>
              putStrLn . show $ length input

longer : String -> String -> Nat
longer f s = max (length f) (length s)

printLonger : IO ()
printLonger = do
  putStr "First String: "
  f <- getLine
  putStr "Second String: "
  s <- getLine
  putStr . show $ longer f s

printLongerBind : IO ()
printLongerBind =
  putStr "First String: " >>=
  \_ => getLine >>=
  \f => putStr "Second String: " >>=
  \_ => getLine >>=
  \s => putStr . show $ longer f s

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
    then pure (Just (cast input))
    else pure Nothing
