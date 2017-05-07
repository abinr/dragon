import Data.Vect

allLengths : Vect len String -> Vect len Nat
allLengths [] = []
allLengths (word :: words) = length word :: allLengths words


xor : Bool -> Bool -> Bool
xor False y = y
xor True y = not y

isEven : Nat -> Bool
isEven Z = True
isEven (S k) = not (isEven k)

insert : Ord elem => (x : elem) -> (xs : Vect len elem) -> Vect (S len) elem
insert x [] = [x]
insert x (y :: xs) =
 case x < y of
   False => y :: insert x xs
   True => x :: y :: xs

insSort : Ord elem => Vect n elem -> Vect n elem
insSort [] = []
insSort (x :: xs) = 
  let
    xsSorted = insSort xs
  in
    insert x xsSorted

total my_length : List a -> Nat
my_length [] = 0
my_length (x :: xs) = 1 + my_length xs

total my_reverse : List a -> List a
my_reverse [] = []
my_reverse (x :: xs) = my_reverse xs ++ [x]

total my_map : (a -> b) -> List a -> List b
my_map f [] = []
my_map f (x :: xs) = f x :: my_map f xs

total my_vect_map : (a -> b) -> Vect n a -> Vect n b
my_vect_map f [] = []
my_vect_map f (x :: xs) = f x :: my_vect_map f xs

test : Bool
test =
   all id (the (List Bool) [
    my_length [1..10] == 10
  , my_reverse [1..10] == [10..1]
  , my_map (* 2) [1..10] == [2,4..20]
  , my_vect_map length ["Hot", "Dog", "Jumping", "Frog"] == [3, 3, 7, 4]
  ])
