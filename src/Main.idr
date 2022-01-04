module Main

import Data.String
import Data.List
import Data.Vect
import System.REPL

import CrashCourse

pythagoras : Double -> Double -> Double
pythagoras x y = sqrt (square x + square y)
    where
        square : Double -> Double
        square x = x * x

wordsCount : String -> Nat
wordsCount x = length (words x)

isEven : Nat -> Bool
isEven 0 = True
isEven (S k) = not (isEven k)

FiveInts : Vect 5 Int
FiveInts = [0, 1, 2, 3, 5]

total allLengths : Vect len String -> Vect len Nat
allLengths [] = []
allLengths (x :: xs) = length x :: allLengths xs

total insert : Ord e => 
         (x : e) -> (xsSorted : Vect k e) -> Vect (S k) e
insert x [] = [x]
insert x (y :: ys) = case x < y of
                          False => y :: insert x ys
                          True => x :: y :: ys

total insSort : Ord e => Vect n e -> Vect n e
insSort [] = []
insSort (x :: xs) =
    let xsSorted = insSort xs in
        insert x xsSorted

-- All about data definitions
data Bool = False | True

data Direction = Up | Right | Down | Left

turnClockWise : Direction -> Direction
turnClockWise Up = Right
turnClockWise Right = Down
turnClockWise Down = Left
turnClockWise Left = Up


{- In F#, below is basically -
 - type Shape = 
        | Triangle of double * double
        | Rectangle of double * double
        | Circle of double
 -}
data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

-- To test above, example: area $ Triangle 5.2 4.2 or area $ Circle 4.2 22.2


-- Recursive types


{- In F#, this could be -
 - type Picture =
        | Primitive of double  // no shape defined, so use double as placeholder
        | Combine of Picture * Picture
        | Rotate of double * Picture
        | Translate of double * double * Picture
 -}
data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

rectangle : Picture
rectangle = Primitive $ Rectangle 10 10

circle : Picture
circle = Primitive $ Circle 5

triangle : Picture
triangle = Primitive $ Triangle 10 10


testPicture : Picture
testPicture = Combine 
                (Translate 5 5 rectangle)
                (Combine (Translate 35 5 circle) (Translate 15 25 triangle))

-- These are for auto type def fill
%name Shape shape, shape1, shape2
%name Picture pic, pic1, pic2

pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic pic1) = pictureArea pic + pictureArea pic1
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic


-- Generic types

-- Too sleepy to figure out the implementation
-- FIXME: Continue this later when coffee is on the line.
biggestTriangle : Picture -> Maybe Double
biggestTriangle pic = 
    case pic of
         Primitive s => case s of
                   Triangle _ _ => Just $ pictureArea pic
                   _            => Nothing
         _           =>  Nothing


-- Dependent types (finally a much more precise data type definition constructor)
data PowerSource = Petrol | Pedal
data Vehicle: PowerSource -> Type where
     Bicycle: Vehicle Pedal
     Car: (fuel: Nat) -> Vehicle Petrol
     Minivan: (fuel: Nat) -> Vehicle Petrol

wheels: Vehicle p -> Nat
wheels Bicycle = 2
wheels (Car fuel) = 350
wheels (Minivan fuel) = 500

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Minivan fuel) = Minivan 150

mappend : Vect n e -> Vect m e -> Vect (n + m) e
mappend [] ys = ys
mappend (x :: xs) ys = x :: mappend xs ys


main : IO ()
main = do
    putStrLn "Hello from Idris2 Playground about types and pure functions"
