--First function
subSeq :: String -> String -> Bool
subSeq [] _ = True
subSeq _ [] = False
subSeq (x:xs) (y:ys)
  | x == y    = subSeq xs ys
  | otherwise = subSeq (x:xs) ys


--Second function
subWord :: String -> String -> Bool
subWord [] _ = True
subWord _ [] = False
subWord x (y:ys)
  | x == take (length x) (y:ys) = True
  | otherwise                   = subWord x ys 


--Third function
--- Case A
isMatrix :: [[a]] -> Bool
isMatrix [] = False
isMatrix (x:xa:xb)
  | length x == length xa && not (null x)  = isMatrix (xa:xb)
  | otherwise                              = False
isMatrix (x:xs)
  | null x     = False
  | otherwise  = True

--- Case B
isSquareMatrix :: [[a]] -> Bool 
isSquareMatrix [] = False
isSquareMatrix (x:xs)
  | isMatrix (x:xs) && length (x:xs) == length x = True
  | otherwise                                    = False

--- Case C
addable :: [[a]] -> [[a]] -> Bool
addable [] _ = False
addable _ [] = False
addable (x:xs) (y:ys) 
  | isMatrix (x:xs) && isMatrix (y:ys) && length x == length y && length (x:xs) == length (y:ys) = True
  | otherwise                                                                                    = False

--- Case D
sumRow :: [Int] -> [Int] -> [Int]
sumRow (x:xs) (y:ys) = (x + y):sumRow xs ys
sumRow _ _           = []  

addMatrixHelper :: [[Int]] -> [[Int]] -> [[Int]]
addMatrixHelper (x:xs) (y:ys) = sumRow x y:addMatrixHelper xs ys
addMatrixHelper _ _           = []

addMatrices :: [[Int]] -> [[Int]] -> [[Int]]
addMatrices (x:xs) (y:ys)
  | addable (x:xs) (y:ys) = addMatrixHelper (x:xs) (y:ys)
  | otherwise             = []
addMatrices _ _           = []

--- Case E
multiplyable :: [[a]] -> [[a]] -> Bool
multiplyable (x:xs) (y:ys)
  | isMatrix(x:xs) && isMatrix(y:ys) && length (y:ys) == length x = True
  | otherwise                                                     = False
multiplyable _ _ = False

--- Case F
calculateRowElement :: [Int] -> [[Int]] -> Int -> Int
calculateRowElement (x:xs) (y:ys) pos = (x * y!!pos) + calculateRowElement xs ys pos
calculateRowElement _ _ _             = 0

calculateRow :: [Int] -> [[Int]] -> Int -> [Int]
calculateRow x (y:ys) pos
  | pos < length y = calculateRowElement x (y:ys) pos:calculateRow x (y:ys) (pos + 1)
  | otherwise      = []
calculateRow _ _ _ = []

multiplyMatrixHelper :: [[Int]] -> [[Int]] -> [[Int]]
multiplyMatrixHelper (x:xs) (y:ys) = calculateRow x (y:ys) 0:multiplyMatrixHelper xs (y:ys)
multiplyMatrixHelper _ _           = []

multiplyMatrices :: [[Int]] -> [[Int]] -> [[Int]]
multiplyMatrices (x:xs) (y:ys)
  | multiplyable (x:xs) (y:ys) = multiplyMatrixHelper (x:xs) (y:ys)
  | otherwise                  = []
multiplyMatrices _ _           = []
