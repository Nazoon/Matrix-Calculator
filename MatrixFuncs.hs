module MatrixFuncs (
    getRow,
    getCol,
    setVecAt,
    setMatAt,
    setRow,
    setCol,
    transpose,
    addVectors,
    addMats,
    vecDotProd,
    matDotProd,
    printRow,
    printMatrix
) where

import Data.Char

-- | Utility

appendToNthList :: [[a]] -> a -> Int -> [[a]]
appendToNthList [] _ _ = []
appendToNthList (l : ls) x 0 = ((l ++ [x]) : ls)
appendToNthList (l : ls) x i = l : (appendToNthList ls x (i - 1))

-- This one takes a list of lists, and a list of the same length, and 1-to-1 appends elements into the list of lists
-- You can think of it like appending a column to a matrix. Naturally useful for matrix transposition as seen below.
distribute :: [[a]] -> [a] -> [[a]]
distribute [] _ = []
distribute _ [] = []
distribute (l : ls) (x : xs) = (l ++ [x]) : (distribute ls xs)

multiplyListHelper :: a -> Int -> [a] -> [a]
multiplyListHelper _ 0 acc = acc
multiplyListHelper l n acc = multiplyListHelper l (n - 1) (acc ++ [l])

multiplyList :: a -> Int -> [a]
multiplyList l n = multiplyListHelper l n []

-- | Getters and Setters

getRow :: [[a]] -> Int -> [a]
getRow m i = m !! i

getColHelper :: [[a]] -> Int -> [a] -> [a]
getColHelper [] _ acc = acc
getColHelper (row : rows) i acc = getColHelper rows i (acc ++ [(row !! i)])

getCol :: [[a]] -> Int -> [a]
getCol m i = getColHelper m i []

getMatAt :: [[a]] -> (Int, Int) -> a
getMatAt m (row,col) = (getRow m row) !! col

setRow :: [[a]] -> [a] -> Int -> [[a]]
setRow (row : rows) newRow 0 = (newRow : rows)
setRow (row : rows) newRow i = (row : (setRow rows newRow (i - 1)))

setCol :: [[a]] -> [a] -> Int -> [[a]]
setCol m newCol i = transpose (setRow (transpose m) newCol i)

setVecAt :: [a] -> a -> Int -> [a]
setVecAt (x : xs) y 0 = (y : xs)
setVecAt (x : xs) y i = (x : (setVecAt xs y (i - 1)))

setMatAt :: [[a]] -> a -> (Int, Int) -> [[a]]
setMatAt m x (i,j) =
    let
        row = getRow m i
        newRow = setVecAt row x j
    in
        setRow m newRow i

-- | Matrix transposition

transposeHelper :: [[a]] -> [[a]] -> [[a]]
transposeHelper [] acc = acc
transposeHelper (row : rows) acc = transposeHelper rows (distribute acc row)

transpose :: [[a]] -> [[a]]
transpose m = transposeHelper m (multiplyList [] (length (head m)))

-- | Vector addition

addVecHelper :: (Num a) => [a] -> [a] -> [a] -> [a]
addVecHelper [] [] acc = acc
addVecHelper (u : us) (v : vs) acc = addVecHelper us vs (acc ++ [(u + v)])

addVectors :: (Num a) => [a] -> [a] -> [a]
addVectors u v = addVecHelper u v []

-- | Matrix addition

addMatHelper :: (Num a) => [[a]] -> [[a]] -> [[a]] -> [[a]]
addMatHelper [] [] acc = acc
addMatHelper (m : ms) (n : ns) acc = addMatHelper ms ns (acc ++ [(addVectors m n)])

addMats :: (Num a) => [[a]] -> [[a]] -> [[a]]
addMats m n = addMatHelper m n []

-- | Dot product

vecDotHelper :: (Num a) => [a] -> [a] -> a -> a
vecDotHelper [] [] acc = acc
vecDotHelper (u : us) (v : vs) acc = vecDotHelper us vs (acc + (u * v))

vecDotProd :: (Num a) => [a] -> [a] -> a
vecDotProd u v = vecDotHelper u v 0

matDotProd :: (Num a) => [[a]] -> [[a]] -> [[a]]
matDotProd [] _ = []
matDotProd _ [] = []
matDotProd m n =
    let
        rows = length m
        cols = length (head n)
    in
        [[vecDotProd (getRow m i) (getCol n j) | j <- [0..cols-1]] | i <- [0..rows-1]]

-- | Identity matrix

identityHelper :: Int -> [[Int]] -> [[Int]]
identityHelper 0 acc = setMatAt acc 1 (0,0)
identityHelper i acc = identityHelper (i-1) (setMatAt acc 1 (i,i))

identity :: Int -> [[Int]]
identity 0 = []
identity n =
    let
        blank = multiplyList (multiplyList 0 n) n
    in
        identityHelper (n-1) blank

-- | Print functions

joinWithSpacesHelper :: [[Char]] -> [Char] -> [Char]
joinWithSpacesHelper [] acc = acc
joinWithSpacesHelper (s : ss) [] = joinWithSpacesHelper ss s
joinWithSpacesHelper (s : ss) acc = joinWithSpacesHelper ss (acc ++ " " ++ s)

joinWithSpaces :: [[Char]] -> [Char]
joinWithSpaces ss = joinWithSpacesHelper ss []

printRow :: Show a => [a] -> IO ()
printRow row = putStrLn (joinWithSpaces (map show row))

printMatrix :: Show a => [[a]] -> IO ()
printMatrix [] = putStr ""
printMatrix (row : rows) = (printRow row) >> printMatrix rows

m :: [[Int]]
m = 
    [
        [1,2],
        [3,4],
        [5,6]
    ]