module CrashCourse

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

isSingleton : Bool -> Type
isSingleton False = Nat
isSingleton True = List Nat

makeSingle : (x : Bool) -> isSingleton x
makeSingle False = 0
makeSingle True = []

greet : IO ()
greet = do
    putStr "What is your name? "
    name <- getLine
    putStrLn ("Hello " ++ name ++ "!")