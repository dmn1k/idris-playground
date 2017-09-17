import Data.Vect

createEmpties : Vect m (Vect 0 elem)
createEmpties = replicate _ [] 

transposeMat : Vect n (Vect m elem) -> Vect m (Vect n elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTransposed = transposeMat xs in
                             zipWith (::) x xsTransposed


addRow : Num a => (x : Vect m a) -> (y : Vect m a) -> Vect m a
addRow [] [] = []
addRow (x :: xs) (y :: ys) = (x + y) :: addRow xs ys 

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = let remainingResult= addMatrix xs ys in
                                    addRow x y :: remainingResult

{- 
    [1, 2]   [1, 2]   [7, 10]
    [3, 4] x [3, 4] = [15, 18]
    [5, 6]            [23, 34]

    Transposed:

    [1, 2]   [1, 3]
    [3, 4] x [2, 4] 
    [5, 6]
-}

buildRowElement : Num a => Vect m a -> Vect m a -> a
buildRowElement [] [] = 0 
buildRowElement (x :: xs) (y :: ys) = x * y + buildRowElement xs ys 

buildRow : Num a => Vect m a -> Vect y (Vect m a) -> Vect y a
buildRow [] [] = []
buildRow xRow [] = []
buildRow xRow (y :: ys) = buildRowElement xRow y :: buildRow xRow ys 

multMatrix : Num a => Vect x (Vect m a) -> Vect m (Vect y a) -> Vect x (Vect y a)
multMatrix [] [] = []
multMatrix [] matY = []
multMatrix (x :: xs) matY = let transposedY = transposeMat matY in
                                buildRow x transposedY :: multMatrix xs matY
