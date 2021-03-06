-- problems 54a-60
-- https://wiki.haskell.org/99_questions/54A_to_60

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
leaf x = Branch x Empty Empty


-- problem 54a
-- Haskell's type system is such that it requires no solution


-- problem 55
-- methodology (I should probably start documenting these):
-- the only cbaltree of size 0 is Empty
-- for higher size (n+1):
--   even n: each left/right cycles through all possible n/2 subtrees
--   odd n: left/right cycle through all n/2, n/2+1 trees
cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n
  | even n_children =
    let subtrees = cbalTree child_size
    in [Branch 'x' l r | l <- subtrees, r <- subtrees]
  | otherwise =
    let subtrees_s = cbalTree child_size        -- smaller
        subtrees_l = cbalTree $ child_size + 1  -- larger
    in [Branch 'x' l r | l <- subtrees_s, r <- subtrees_l]
    ++ [Branch 'x' l r | l <- subtrees_l, r <- subtrees_s]
  where n_children = n - 1
        child_size = quot n_children 2


-- problem 56
-- methodology:
-- we define mirror trees A, B:
--   preorder A and postorder B traversals match
-- a tree is symmetric if its subtrees are mirror
mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror Empty _ = False
mirror _ Empty = False
mirror (Branch _ l1 r1) (Branch _ l2 r2) =
  (mirror l1 r2) && (mirror r1 l2)

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = mirror l r


-- problem 57
-- we will assume lists have no duplicate integers
-- methodology:
-- we fold over the integer list with an insert function
-- the insert function replaces the appropriate subtree
-- (right or left) with the tree with the new node inserted
insert_node :: Ord a => Tree a -> a -> Tree a
insert_node Empty node = leaf node
insert_node (Branch val left right) node =
  if node < val
    then (Branch val (insert_node left node) right)
  else (Branch val left (insert_node right node))

construct :: Integral a => [a] -> Tree a
construct =
  foldl insert_node Empty


-- problem 58
-- methodology:
-- we use brute force: make all possible trees and then filter by symmetry
symCbalTrees :: Int -> [Tree Char]
symCbalTrees = (filter symmetric) . cbalTree


-- problem 59
-- methodology:
-- the only possible hbalTree of size 0 is Empty
-- the only possible hbalTree of size 1 is (leaf 'x')
-- otherwise, left and right subtrees must have fixed heights
hbalTree :: a -> Int -> [Tree a]
hbalTree _ 0 = [Empty]
hbalTree x 1 = [leaf x]
hbalTree x h =
  let h1 = hbalTree x (h-1)
      h2 = hbalTree x (h-2)
  in [Branch x l r | l <- h2, r <- h1] ++
     [Branch x l r | l <- h1, r <- h1] ++
     [Branch x l r | l <- h1, r <- h2]

-- problem 60
-- methodology:
-- we take the minNodes(h) function from the 122 homework to construct a
-- maxHeight(n) function
-- then we use hbalTree(x, h)
minNodes :: Int -> Int
minNodes 0 = 0
minNodes 1 = 1
minNodes h = 1 + (minNodes $ h-1) + (minNodes $ h-2)

maxHeight :: Int -> Int  -- this is a very inefficient method, but it's simple
maxHeight 0 = 0
maxHeight 1 = 1
maxHeight n =
  let prev = zip [0..] $ takeWhile (< n) $ map minNodes [0..]
      pmax = foldl (\((ha, a), (hb, b)) (hx, x) ->
                     if x + b < n
                        then ((hb, b), (hx, x))
                     else ((ha, a), (hb, b))) ((0, 0), (0, 0)) prev
  in (+ 1) . fst . snd $ pmax

size :: Tree a -> Int
size Empty = 0
size (Branch _ left right) = 1 + (size left) + (size right)

hbalTreeNodes :: a -> Int -> [Tree a]  -- again, inefficient but it works!
hbalTreeNodes _ 0 = [Empty]
hbalTreeNodes x 1 = [leaf x]
hbalTreeNodes x n =
  let heights = [0..(maxHeight n)]
      candidates = foldl (++) [] $ map (hbalTree x) heights
  in filter ((== n) . size) candidates
