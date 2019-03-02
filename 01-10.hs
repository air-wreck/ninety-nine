-- problems 1-10
-- https://wiki.haskell.org/99_questions/1_to_10

-- problem 1
myLast :: [a] -> a
myLast xs@(x:_) = foldl (\acc y -> y) x xs

-- problem 2
myButLast :: [a] -> a
myButLast (x:_:[]) = x
myButLast (_:xs) = myButLast xs

-- problem 3
elementAt :: [a] -> Int -> a
elementAt xs i
  | i == length xs = myLast xs
  | otherwise      = elementAt (init xs) i

-- problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- problem 5
myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x:acc) []

-- problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs =
  and $ map (\(x,y) -> x == y) $ zip xs $ reverse xs

-- problem 7
data NestedList a = Elem a | List [NestedList a] deriving (Eq, Show)
flatten :: (Eq a) => NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = foldl (\acc x -> acc ++ (flatten x)) [] xs

-- problem 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:[]) = [x]
compress (x1:x2:[])
  | x1 == x2  = [x1]
  | otherwise = [x1, x2]
compress (x1:x2:xs)
  | x1 == x2  = compress (x2:xs)
  | otherwise = x1 : compress (x2:xs)

-- problem 9
pack :: (Eq a) => [a] -> [[a]]
pack [] = [[]]
pack (x:[]) = [[x]]
pack (x1:x2:[])
  | x1 == x2  = [[x1,x2]]
  | otherwise = [[x1],[x2]]
pack (x1:x2:xs)
  | x1 == x2  = appendToFirst (pack (x2:xs)) x1
  | otherwise = [[x1]] ++ pack (x2:xs)
appendToFirst :: [[a]] -> a -> [[a]]
appendToFirst xs y = [y : head xs] ++ tail xs

-- problem 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = [(length x, x!!0) | x <- pack xs]
