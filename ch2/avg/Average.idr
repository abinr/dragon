module Average

||| Calculate the average length of words in a string.
||| @str a string contains words separated by whitespace
export
averageWordLength : ( str : String) -> Double
averageWordLength str =
  let
    totalLength = cast . sum . (map length) . words
    totalWords = cast . length . words
  in
    totalLength str / totalWords str

test : Bool
test =
  4.2 == averageWordLength "How long are these words?"



