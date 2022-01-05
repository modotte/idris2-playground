module CrashCourse

import Data.String
import Data.List
import Data.Vect
import System.REPL

export
xor : Bool -> Bool -> Bool
xor False y = y
xor True y = not y

export
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


record Person where
    constructor MkPerson
    firstName, middleName, lastName : String
    nickName: String
    age : Int

jake : Person
jake = MkPerson "Jake" "Hammond" "Jameson" "Jarl" 25

{-
- In F#, we do { jake with firstName = "Hammond"; nickName = "Sickle" }
-}
jakesTwinBrother : Person
jakesTwinBrother = { firstName := "Hammond", nickName := "Sickle" } jake

