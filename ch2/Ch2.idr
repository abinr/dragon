module Ch2


export
palindrome : Nat -> String -> Bool
palindrome n str =
  let
    ls = toLower str
  in
    case length str > n of
      True => ls == reverse ls
      False => False

testPalindrome : Bool
testPalindrome =
  let
    one = not $ palindrome 10 "racecar"
    two = not $ palindrome 0 "race car"
    three = palindrome 0 "Racecar"
    four = palindrome 10 "able was i ere i saw elba"
    five = palindrome 5 "racecar"
  in
    all id [one, two, three, four, five]

export
counts : String -> (Nat, Nat)
counts str =
  let
    ws = length . words
  in
    (ws str, length str)

testCounts : Bool
testCounts =
 let
   one = counts "" == (0,0)
   two = counts "H" == (1,1)
   three = counts "Hello, Idris world!" == (3, 19)
 in
   all id [one, two, three]

topTen : Ord a => List a -> List a
topTen =
  take 10 . sort

testTopTen : Bool
testTopTen =
  let
    one = topTen [] == (the (List Int) [])
    two = topTen [1] == [1]
    three = topTen [1..100] == [1..10]
  in
    all id [one, two, three]

overLength : Nat -> List String -> Nat
overLength n t =
  length $ filter (> n) $ map length t

testOverLength : Bool
testOverLength =
  let
    one = overLength 0 [] == 0
    two = overLength 0 ["h"] == 1
    three = overLength 3 ["One", "Two", "Three", "Four"] == 2
  in
    all id [one, two, three]

