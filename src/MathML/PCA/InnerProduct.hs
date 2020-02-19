module MathML.PCA.InnerProduct where

import Numeric.LinearAlgebra as LA

-- ^ compute inner product based on a given matrix
innerProduct :: LA.Matrix R -> LA.Vector R -> LA.Vector R -> R
innerProduct a x y = (x <# a) <.> y

lengthBy :: LA.Matrix R -> LA.Vector R -> R
lengthBy a x = sqrt $ innerProduct a x x

radBy :: LA.Matrix R -> LA.Vector R -> LA.Vector R -> R
radBy a x y = acos $ innerProduct a x y / (lengthBy a x * lengthBy a y)

ones :: Int -> LA.Vector R
ones n = LA.vector $ replicate n 1


ip :: LA.Matrix R -> LA.Vector R -> LA.Vector R -> R
ip  = innerProduct

id2, id3 :: Matrix R
id2 = ident 2
id3 = ident 3

v123, v321 :: LA.Vector R
v123 = (LA.vector [1, 2, 3])
v321 = (LA.vector [3, 2, 1])

ex1 :: Double
ex1 = do
    let x = vector [1, 1]
    let y = vector [-1, 1]
    let a = LA.fromLists [[2, -1], [-1, 4]]
    radBy a x y

ex2 :: Double
ex2 = do
    let x = vector [0, -1]
    let y = vector [1, 1]
    let a = LA.fromLists [[1, -0.5], [-0.5, 5]]
    radBy a x y

ex3 :: Double
ex3 = do
    let x = vector [2, 2]
    let y = vector [-2, -2]
    let a = LA.fromLists [[2, 1], [1, 4]]
    radBy a x y

ex4 :: Double
ex4 =
    let x = vector [1, 1]
        y = vector [1, -1]
        a = LA.fromLists [[1, 0], [0, 5]]
    in radBy a x y

ex5 :: Double
ex5 =
    let x = vector [1, 1, 1]
        y = vector [2, -1, 0]
        a = LA.fromLists [[1, 0, 0], [0, 2, -1], [0, -1, 3]]
    in radBy a x y

lecture01 :: Double
lecture01 =
    let x = vector [1, 1]
        y = vector [-1, 1]
        a = LA.fromLists [[2, 0], [0, 1]]
    in radBy a x y
main :: IO ()
main = do
    print ex1
    print ex2
    print ex3
    print ex4
    print ex5
    print lecture01

tests :: IO ()
tests = do
    print $ "dot with identity:  " ++ show (ip id3 v123 v321)
    print $ "dot scaled 2x:      " ++ show (lengthBy id3 (ones 3))
    print $ "90deg (0,1) (1, 0): " ++ show (radBy id2 (vector [0, 1]) (vector [1, 0]))
    print $ "00deg (0,1) (0, 1): " ++ show (radBy id2 (vector [0, 1]) (vector [0, 1]))
