module Main

import Ch2

showPalindrome : String -> String
showPalindrome str =
  (show $ palindrome 0 str) ++ "\n"

main : IO ()
main =
  repl "Enter a string: " showPalindrome
