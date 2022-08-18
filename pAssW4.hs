--First function
isPowerOfTwo :: Int -> Bool
isPowerOfTwo n
  | n <= 0 				 = False
	| n == 1 				 = True
	| n `mod` 2 == 0 = isPowerOfTwo (n `div` 2)
	| otherwise 		 = False

f1 :: [Int] -> [Int]
f1 [] = []
f1 (x:xs) 
	| isPowerOfTwo(x) = (2*x):(f1 xs)
	| otherwise 			= 0:(f1 xs)

	
--Second function
f2Helper :: Int -> Int -> Char
f2Helper x y
	| x > y			= 'a'
	| otherwise = 'b'

f2 :: [Int] -> [Char]
f2 l = zipWith (f2Helper) l  [0..(length l - 1)]


--Third function
f3 :: [Int] -> [Int]
f3 [] = []
f3 [x] = [x]
f3 (x:y:xs)  
	| x /= y    = x:(f3 (y:xs))
	| otherwise = f3(y:xs)

--Fourth function
f4 :: [Int] -> [[Int]]