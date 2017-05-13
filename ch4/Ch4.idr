data Direction 
  = North
  | South
  | East
  | West

turnClockwise : Direction -> Direction
turnClockwise North = East
turnClockwise South = West
turnClockwise East = South
turnClockwise West = North

||| Represents Shapes
data Shape
  = |||A triangle, with its base and height
    Triangle Double Double
  | ||| A rectangle with its length and width
    Rectangle Double Double
  | ||| A circle with its radius
    Circle Double

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

data Picture
  = Primitive Shape
  | Combine Picture Picture
  | Rotate Double Picture
  | Translate Double Double Picture

rectangle : Picture
rectangle = Primitive (Rectangle 20 20)

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (Triangle 10 10)

testPicture : Picture
testPicture = Combine (Translate 5 5 rectangle)
              (Combine (Translate 35 5 circle) 
              (Translate 15 25 triangle))

pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine x y) = pictureArea x + pictureArea y
pictureArea (Rotate angle shape) = pictureArea shape
pictureArea (Translate x y shape) = pictureArea shape




bigTriHelp : Maybe Double -> Maybe Double -> Maybe Double
bigTriHelp Nothing Nothing = Nothing
bigTriHelp Nothing (Just y) = Just y
bigTriHelp (Just x) Nothing = Just x
bigTriHelp (Just x) (Just y) = 
  Just $ max x y

total
biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive (Triangle x y)) = Just $ area (Triangle x y)
biggestTriangle (Primitive _) = Nothing
biggestTriangle (Combine x y) = bigTriHelp (biggestTriangle x) (biggestTriangle y)
biggestTriangle (Rotate x y) = biggestTriangle y
biggestTriangle (Translate x y z) = biggestTriangle z

testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3))
                   (Primitive (Triangle 2 4))

testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3))
                   (Primitive (Circle 4))

data Expr
  = Val Int
  | Add Expr Expr
  | Minus Expr Expr
  | Times Expr Expr

evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add x y) = evaluate x + evaluate y
evaluate (Minus x y) = evaluate x - evaluate y
evaluate (Times x y) = evaluate x * evaluate y

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing y = y
maxMaybe x Nothing = x
maxMaybe (Just x) (Just y) = Just (max x y)
