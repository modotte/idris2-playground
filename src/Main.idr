module Main

import Data.String
import Data.List
import Data.Vect
import System.REPL

xor : Bool -> Bool -> Bool
xor False y = y
xor True y = not y

longer : String -> String -> Nat
longer x y 
    = let l1 = length x
          l2 = length y in
          if l1 > l2 then l1 else l2

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





main : IO ()
main = do
    putStrLn "Hello from Idris2!"
