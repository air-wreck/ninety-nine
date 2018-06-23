-- problems 46-50
-- https://wiki.haskell.org/99_questions/46_to_50

import Data.List
import Data.Maybe

-- problem 46
not' :: Bool -> Bool
not' a = if a then False else True
and' :: Bool -> Bool -> Bool
and' a b = if a then b else False
or' :: Bool -> Bool -> Bool
or' a b = if a then True else b
nand' :: Bool -> Bool -> Bool
nand' a = not' . (and' a)
nor' :: Bool -> Bool -> Bool
nor' a = not' . (or' a)
xor' :: Bool -> Bool -> Bool
xor' a b = if a then not' b else b
-- i think impl/2 is supposed to mean "implies"???
-- e.g. impl' (\a b -> and' a b) True False = False
impl' :: (Bool -> Bool -> Bool) -> Bool -> Bool -> Bool
impl' f a b = f a b
-- a little different from the example one, but no big deal
-- e.g. equ' (\a b -> not' $ and' a b) (nand') = True
equ' :: (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool
equ' f g = foldl (&&) True
           [not' $ xor' (impl' f a b) (impl' g a b)
             | a <- [True, False], b <- [True, False]]
table :: (Bool -> Bool -> Bool) -> [Char]
table f = foldl (++) []
  $ map (\(a, b, res) -> (show a) ++ " " ++
                         (show b) ++ " " ++
                         (show res) ++ "\n")
        [(a, b, f a b) | a <- [True, False], b <- [True, False]]

-- problem 47
-- i think haskell can already make functions of two variables infix
-- by using backticks, so this isn't necessary
-- e.g. putStr $ table (\a b -> a `and'` (a `or'` not' b))

-- problem 48
-- e.g. putStr $ tablen 3 (\[a,b,c] -> a `or'` b `or'` c)
tablen :: Int -> ([Bool] -> Bool) -> [Char]
tablen n f =
  foldl (\acc x -> acc ++ x ++ "\n") ""
    $ map (\input ->
      (foldl (\acc x ->
        acc ++ (show x) ++ " ") "" input ++ (show $ f input))) inputs
  where inputs = foldl (\acc x -> foldl (++) []
          $ map (\y -> [True:y, False:y]) acc) [[True], [False]] [2..n]

-- problem 49
-- n = 1: C(1) = ['0', '1']
-- n = 2: C(2) = ['00', '01', '11', '10']
-- n = 3: C(3) = ['000', '001', '011', '010', '110', '111', '101', '100']
-- in order, add a '0' in front of each thing from previous
-- in reverse, add a '1' in front of each thing previous
gray :: Int -> [[Char]]
gray 1 = ["0", "1"]
gray n = (cons '0' prev) ++ (cons '1' $ reverse prev)
  where prev = gray (n-1)
        cons = map . (:)
-- memoization in Haskell looks hard, so i'll do that later

-- problem 50
-- this is ugly and the output is in a weird order, but it works so i won't
--   touch it
data Tree a = Empty | Node (Maybe a) Int (Tree a) (Tree a) deriving Show
newqueue :: [Tree a] -> [Tree a]
newqueue xs
  | length xs == 1 = xs
  | otherwise      =
    let sorted = sortBy (\(Node _ f1 _ _) (Node _ f2 _ _) -> compare f1 f2) xs
      in newqueue $ (Node Nothing ((freq (sorted!!0)) + (freq (sorted!!1)))
        (sorted!!0) (sorted!!1)):(drop 2 sorted)
    where freq (Node _ f _ _) = f
travel :: Tree a -> [Char] -> [(a, String)]
travel node code = (foldl (++) [] $ map (\(n, c) ->
    if notEmpty n
      then travel n (code++c)
    else [])
    [(left node, "0"), (right node, "1")])
  ++ (if isJust $ val node then [(fromJust $ val node, code)] else [])
    where left (Node _ _ l _) = l
          right (Node _ _ _ r) = r
          val (Node v _ _ _) = v
          notEmpty (Empty) = False
          notEmpty (Node _ _ _ _) = True
huffman :: [(a, Int)] -> [(a, [Char])]
huffman xs = travel ((newqueue $ map (\(val,freq) ->
  Node (Just val) freq Empty Empty) xs)!!0) ""
