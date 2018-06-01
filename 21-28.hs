-- problems 21-28
-- https://wiki.haskell.org/99_questions/21_to_28

import System.Random

-- problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = (take (n-1) xs) ++ [x] ++ (drop (n-1) xs)

-- problem 22
range :: Int -> Int -> [Int]
range start end = takeWhile (<= end) [start..]

-- problem 23
rnd_select :: [a] -> Int -> IO [a]
rnd_select _ 0 = do return []
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
