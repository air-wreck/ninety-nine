-- problems 31-41
-- https://wiki.haskell.org/99_questions/31_to_41

-- problem 31
isPrime :: Int -> Bool
isPrime n
  | n < 2            = False
  | n == 2 || n == 3 = True
  | n `rem` 2 == 0   = False
  | otherwise        = foldl (\acc x -> not (n `rem` x == 0) && acc) True
                     $ takeWhile (<= (ceiling $ sqrt $ fromIntegral n)) [3,5..]

-- problem 32
-- use gcd' to avoid conflicting with Prelude function gcd
gcd' :: Int -> Int -> Int
gcd' p q
  | p `rem` q == 0 = q
  | otherwise      = gcd' q (p `rem` q)

-- problem 33
coprime :: Int -> Int -> Bool
coprime n = (== 1) . gcd n

-- problem 34
totient :: Int -> Int
totient m = length $ filter (coprime m) [1..m]

-- problem 35
primeFactors :: Int -> [Int]
primeFactors n = foldl (\acc p -> acc ++ (take (times n p) $ repeat p)) []
  $ 2:(filter isPrime [3,5..n])
-- auxiliary function find how many times p can be evenly divided into n
times :: Int -> Int -> Int
times n p
  | n `rem` p /= 0 = 0
  | otherwise      = times (n `div` p) p + 1

-- problem 36
prime_factors_mult :: Int -> [(Int, Int)]
prime_factors_mult n = foldr (\p acc ->
  let t = times n p in
    if t /= 0 then (p, t):acc else acc)
  [] $ 2:(filter isPrime [3,5..n])

-- problem 37
phi :: Int -> Int
phi n = round $ foldl (\acc (p, m) -> acc * (p-1) * p ** (m-1)) 1.0
  $ map (\(a, b) -> (fromIntegral a, fromIntegral b)) $ prime_factors_mult n

-- problem 38
-- no solution required

-- problem 39
primesR :: Int -> Int -> [Int]
primesR lo hi = filter isPrime [lo..hi]

-- problem 40
-- if this ever crashes, I will be very happy...
goldbach :: Int -> (Int, Int)
goldbach n = [(a, b) | a <- primesR 1 n, b <- primesR 1 n, a + b == n] !! 0

-- problem 41
-- all goldbach numbers
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList lo hi = map (goldbach) $ filter ((== 0) . (flip rem 2)) [lo..hi]
-- all goldbach numbers with primes greater than 50
-- this is inordinately inefficient
goldbachGreater :: Int -> Int -> Int -> [(Int, Int)]
goldbachGreater lo hi lim = filter (\(a, b) -> a > lim && b > lim)
  $ goldbachList lo hi
