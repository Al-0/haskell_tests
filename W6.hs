data STree a = Nil | Node (STree a) a (STree a) deriving (Eq, Ord, Show)
data LeafTree a = Leaf a | LNode (LeafTree a) (LeafTree a) deriving (Eq, Ord, Show)

--First function
checkPreorder :: Ord a => [a] -> a -> [a] -> Bool
checkPreorder [] _  _ = True
checkPreorder (x:xs) root []
  | x < root  = False
  | otherwise = checkPreorder xs root [x]       
checkPreorder (x:xs) root (s:st)
  | x > s     = checkPreorder (x:xs) s st
  | x < root  = False  
  | otherwise = checkPreorder xs root (x:s:st)            

validPreorder :: Ord a => [a] -> Bool
validPreorder x = checkPreorder x (minimum x) []


--Second function
getNextMax :: Ord a => [a] -> a -> [a]
getNextMax [] _ = []
getNextMax (x:xs) root
  | x <= root = getNextMax xs root
  | otherwise = x:xs

generateSTree :: Ord a => [a] -> a -> a -> STree a
generateSTree [] _ _ = Nil
generateSTree (x:xs) minLimit maxLimit
  | x < minLimit || x > maxLimit = Nil
  | otherwise                    = Node (generateSTree xs minLimit (min x maxLimit))  x (generateSTree (getNextMax xs x) (max x minLimit) maxLimit)

fromPreorder :: Ord a => [a] -> Maybe (STree a)
fromPreorder x
  | validPreorder x = Just (generateSTree x (minimum x) (maximum x))
  | otherwise       = Nothing
-------- I am in physical pain after this one


--Third Function
getInorder :: Ord a => STree a -> [a]
getInorder Nil = []
getInorder (Node tl x tr) = getInorder tl ++ [x] ++ getInorder tr

nextmin :: Ord a => STree a -> a
nextmin t= getInorder t!!1

--Fourth function
leafy :: Eq a => STree a -> Int
leafy Nil = 0 
leafy (Node tl x tr)
  | tl == Nil && tr == Nil = 1
  | otherwise              = leafy tl + leafy tr

--Fifth function
----Case A
searchMinimum :: Ord a => LeafTree a -> a
searchMinimum (Leaf x) = x
searchMinimum (LNode x y) =  min (searchMinimum x) (searchMinimum y)

searcMaximum :: Ord a => LeafTree a -> a
searcMaximum (Leaf x) = x
searcMaximum (LNode x y) =  max (searcMaximum x) (searcMaximum y)

hasSearch :: Ord a => LeafTree a -> Bool
hasSearch (Leaf x) = True
hasSearch (LNode x y)
  | searcMaximum x < searchMinimum y = hasSearch x && hasSearch y
  | otherwise                         = False

----CaseB
search :: Ord a => LeafTree a -> a -> Bool
search (Leaf x) target 
  | x == target = True
  | otherwise   = False
search (LNode x y) target = search x target || search y target
