import Data.Vect

total allLengths : Vect len String -> Vect len Nat
allLengths [] = []
allLengths (word :: words) = length word :: allLengths words

mutual
    isEven : Nat -> Bool
    isEven Z = True 
    isEven (S k) = isOdd k 

    isOdd : Nat -> Bool
    isOdd Z = False 
    isOdd (S k) = isEven k 

