-- First function
f1Helper :: [a] -> Int -> Int -> Int -> [a]
f1Helper [] _ _ _ = []
f1Helper (x:xs) n i idx
  | idx == n*i-1 = f1Helper xs n (i + 1) (idx + 1)   
  | otherwise    = x:f1Helper xs n i (idx + 1)

f1 :: [a] -> Int -> [a]
f1 l n = f1Helper l n 1 0


-- Second function
f2Helper :: [a] -> [a] -> [[a]]
f2Helper [] pre     = [pre]
f2Helper (x:xs) pre = pre:f2Helper xs (pre++[x])  

f2 :: [a] -> [[a]]
f2 l = f2Helper l []


--- Third function
f3Helper :: [a] -> Int -> Int -> [[a]]
f3Helper l i j
  | j > length l = []
  | i >= j        = f3Helper l 0 (j+1)
  | otherwise     = drop i (take j l):f3Helper l (i+1) j 

f3 :: [a] -> [[a]]
f3 l = []:f3Helper l 0 1 


-- Fourth function
f4 :: String -> String -> Bool 
f4 [] _ = True
f4 _ [] = False
f4 (x:xs) (y:ys)
  | x == y    = f4 xs ys
  | otherwise = f4 (x:xs) ys