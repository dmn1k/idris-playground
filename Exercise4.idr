import Data.Vect

data Direction = North | East | South | West

turnClockwise : Direction -> Direction
turnClockwise North = East 
turnClockwise East = South 
turnClockwise South = West 
turnClockwise West = North 

||| Represents shapes
data Shape = ||| A triangle with its base length and height 
             Triangle Double Double
           | ||| A rectangle with length and height
             Rectangle Double Double
           | ||| A circle with radius
             Circle Double

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height 
area (Rectangle length height) = length * height 
area (Circle radius) = pi * radius * radius 

data BSTree : Type -> Type where
     Empty : Ord elem => BSTree elem
     Node : Ord elem => (left : BSTree elem) -> (val : elem) -> (right : BSTree elem) -> BSTree elem

insert : elem -> BSTree elem -> BSTree elem
insert x Empty = Node Empty x Empty 
insert x orig@(Node left val right) = case compare x val of
                                      LT => Node (insert x left) val right 
                                      EQ => orig 
                                      GT => Node left val (insert x right) 


listToTree : Ord a => List a -> BSTree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs) 

treeToList : BSTree a -> List a
treeToList Empty = []
treeToList (Node left val right) = (treeToList left) ++ [val] ++ (treeToList right)

data Expr = Val Int
          | Add Expr Expr
          | Mult Expr Expr
          | Sub Expr Expr


evaluate : Expr -> Int
evaluate (Val x) = x 
evaluate (Add x y) = (+) (evaluate x) (evaluate y) 
evaluate (Mult x y) = (*) (evaluate x) (evaluate y)
evaluate (Sub x y) = (-) (evaluate x) (evaluate y)

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing (Just x) = Just x 
maxMaybe (Just x) Nothing = Just x 
maxMaybe (Just x) (Just y) = case x > y of
                                  True => Just x 
                                  False => Just y 


data PowerSource = Petrol | Pedal

data Vehicle : PowerSource -> Type where
     Bicycle : Vehicle Pedal
     Car : (fuel : Nat) -> Vehicle Petrol
     Bus : (fuel : Nat) -> Vehicle Petrol

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100 
refuel (Bus fuel) = Bus 200 
refuel Bicycle impossible


vecTake : (m : Fin (S n)) -> Vect n a -> Vect (cast m) a
vecTake FZ xs = []
vecTake (FS x) (y :: xs) = y :: vecTake x xs

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n = Z} pos [] [] = Nothing
sumEntries {n = (S len)} pos xs ys = case integerToFin pos (S len) of
                                     Nothing => Nothing
                                     Just idx => Just ( (+) (index idx xs) (index idx ys)) 

