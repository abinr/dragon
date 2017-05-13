module Matrix

import Data.Vect

createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = 
  let
    xsTrans = transposeMat xs 
  in
    zipWith (::) x xsTrans

total 
addMatrix : Num a => Vect n (Vect m a) 
  -> Vect n (Vect m a) 
  -> Vect n (Vect m a)
addMatrix [] [] = []test
test

addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys


dot : Num a => (x : Vect m a) -> (y : Vect m a) -> a
dot x y = sum $ zipWith (*) x y

helper : Num a => Vect n (Vect m a) -> Vect p (Vect m a) -> Vect n (Vect p a)
helper [] ys = []
helper (x :: xs) [] = [] :: helper xs []
helper (x :: xs) (y :: ys) = 
  (map (dot x) (y :: ys)) :: helper xs (y :: ys)

total
multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
multMatrix xs ys = helper xs (transpose ys)

test : Bool
test =
  all id ( the (List Bool)[
    transposeMat [[1,2], [3,4], [5,6]] == [[1,3,5], [2,4,6]]
  , addMatrix [[1,2], [3,4]] [[5,6], [7,8]] == [[6,8], [10, 12]]
  , multMatrix [[1,2], [3,4], [5,6]] [[7,8,9,10], [11, 12, 13, 14]] ==
          [[29, 32, 35, 38]
          ,[65, 72, 79, 86]
          ,[101, 112, 123, 134]]
  ])
