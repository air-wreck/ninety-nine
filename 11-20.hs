-- problems 11-20
-- https://wiki.haskell.org/99_questions/11_to_20

-- problem 11
-- copy over functions from problem 9
data Encoded a = Single a | Multiple Int a deriving (Eq, Show)
encodeModified :: (Eq a) => [a] -> [Encoded a]
encodeModified xs = [if length x == 1
                     then Single (x!!0)
                     else Multiple (length x) (x!!0)
                     | x <- pack xs]
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

-- problem 12
decodeModified :: (Eq a) => [Encoded a] -> [a]
decodeModified = foldl (\acc x -> acc ++ (unpack x)) []
unpack :: (Eq a) => Encoded a -> [a]
unpack (Single x) = [x]
unpack (Multiple len x) = take len $ repeat x

-- problem 13
encodeDirect :: (Eq a) => [a] -> [Encoded a]
encodeDirect (x:xs) = encAux xs x 1
encAux :: (Eq a) => [a] -> a -> Int -> [Encoded a]
encAux [] x len = [toEnc x len]
encAux (x:xs) x0 len
  | x == x0   = encAux xs x (len+1)
  | otherwise = (toEnc x0 len) : (encAux xs x 1)
toEnc :: (Eq a) => a -> Int -> Encoded a
toEnc x len
  | len == 1  = Single x
  | otherwise = Multiple len x

-- problem 14
dupli :: [a] -> [a]
dupli = foldr (\x acc -> x:x:acc) []

-- problem 15
repli :: [a] -> Int -> [a]
repli xs n = foldr (\x acc -> (take n (repeat x)) ++ acc) [] xs

-- problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n
  | length xs `rem` n == 0  = dropEvery (init xs) n
  | otherwise  = (dropEvery (init xs) n) ++ [last xs]

-- problem 17
split :: [a] -> Int -> ([a], [a])
split xs len = foldr (\(i, x) (a1, a2) -> if i <= len
  then (x:a1, a2) else (a1, x:a2)) ([], []) (zip [1..] xs)

-- problem 18
slice :: [a] -> Int -> Int -> [a]
slice xs start end = foldr pick [] (zip [1..] xs)
  where pick (i,x) acc = if i >= start && i <= end then x:acc else acc

-- problem 19
rotate :: [a] -> Int -> [a]
rotate xs n
  | n >= 0 = (drop n xs) ++ (take n xs)
  | otherwise = (drop m xs) ++ (take m xs)
      where m = (length xs) + n

-- problem 20
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs!!(n-1), (take (n-1) xs) ++ (drop n xs))
