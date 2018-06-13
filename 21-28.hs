-- problems 21-28
-- https://wiki.haskell.org/99_questions/21_to_28

import System.Random
import Data.List

-- problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = (take (n-1) xs) ++ [x] ++ (drop (n-1) xs)

-- problem 22
range :: Int -> Int -> [Int]
range start end = takeWhile (<= end) [start..]

-- problem 23
rnd_select :: [a] -> Int -> IO [a]
rnd_select _ 0 = return []
rnd_select xs n = newStdGen >>= (\g ->
  let index = fst $ randomR (0 :: Int, (length xs)-1) g in
    (rnd_select ((take index xs) ++ (drop (index+1) xs)) (n-1)) >>= (\list ->
      return $ (xs!!index) : list))

-- problem 24
diff_select :: Int -> Int -> IO [Int]
diff_select n m = rnd_select [1..m] n

-- problem 25
rnd_permu :: [a] -> IO [a]
rnd_permu xs = rnd_select xs (length xs)

-- problem 26
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations n xs = foldl (++) [] (map (\(i, el) ->
  map (\subel -> el : subel) $ combinations (n-1) (drop (i+1) xs))
    $ zip [0..] xs)

-- problem 27
-- we can get rid of the (Eq a) constraint by zipping with [0..], but whatever
-- part (a)
group3 :: Eq a => [a] -> [[[a]]]
group3 xs = foldl (++) [] $ map (\g2 ->
  map (\g3 -> [g2, g3, filter (\x -> (x `notElem` g2) && (x `notElem` g3)) xs])
    (combinations 3 (filter (\x -> x `notElem` g2) xs))) $ combinations 2 xs
-- part (b)
-- we use the identifier group' to avoid conflicting with group from Data.List
group' :: Eq a => [Int] -> [a] -> [[[a]]]
group' ns xs = foldl (\acc n ->
  if length acc == 0
    then map (\x -> [x]) (combinations n xs)
  else foldl (++) [] $ map (\group ->
    map (\combo -> group ++ [combo])
        (combinations n (filter (\x -> isnt x group) xs))) acc) [] ns
-- auxiliary funtion tests if x isn't in a nested list
isnt :: (Foldable t, Eq a) => a -> t (t a) -> Bool
isnt x = foldl (\acc -> (&& acc) . (notElem x)) True


-- problem 28
-- part (a)
lsort :: [[a]] -> [[a]]
lsort xs = map (\pair -> snd pair)
         $ sortBy (\a b -> compare (fst a) (fst b))
         $ map (\list -> (length list, list)) xs
-- part (b)
lfsort :: [[a]] -> [[a]]
lfsort xs = map (\pair -> snd pair)
          $ sortBy (\a b -> compare (fst a) (fst b))
          $ map (\list -> -- this part looks suspiciously LISP-y
            (length (filter ((== (length list)) . length) xs), list)) xs
