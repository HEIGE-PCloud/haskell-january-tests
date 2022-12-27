type Edge = (String, SuffixTree)
data SuffixTree = Leaf Int | Node [Edge] 
                deriving (Eq, Show)

------------------------------------------------------

isPrefix :: String -> String -> Bool
isPrefix [] _
  = True
isPrefix _ []
  = False
isPrefix (x : xs) (y : ys)
  = x == y && isPrefix xs ys

--Pre: s is a prefix of s'
removePrefix :: String -> String -> String
removePrefix [] ys
  = ys
removePrefix (x : xs) (y : ys)
  = removePrefix xs ys

suffixes :: [a] -> [[a]]
suffixes []
  = []
suffixes xxs@(_ : xs)
  = xxs : suffixes xs

isSubstring :: String -> String -> Bool
isSubstring xs ys
  = or (map (isPrefix xs) (suffixes ys))

-- findSubstrings :: String -> String -> [Int]
findSubstrings xs ys
  = map fst fps
  where
    ps = zip [0..] (suffixes ys)
    fps = filter ((isPrefix xs) . snd) ps

------------------------------------------------------

getIndices :: SuffixTree -> [Int]
getIndices (Leaf x)
  = [x]
getIndices (Node xs)
  = concatMap getIndices (map snd xs)

partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition [] []
  = ([], [], [])
partition [] ys
  = ([], [], ys)
partition xs []
  = ([], xs, [])
partition xxs@(x : xs) yys@(y : ys)
  | x /= y = ([], xxs, yys)
  | otherwise = (x : xs', ys', zs')
    where
      (xs', ys', zs') = partition xs ys

findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' [] (Leaf y)
  = [y]
findSubstrings' _  (Leaf y)
  = []
findSubstrings' xs (Node ys)
  = concatMap (match xs) ys
    where
      match :: String -> Edge -> [Int]
      match s (a, st)
        | null y = getIndices st         -- s is a's prefix
        | null z = findSubstrings' y st  -- a is s's prefix
        | otherwise = []
        where
          (x, y, z) = partition s a

------------------------------------------------------
insert :: (String, Int) -> SuffixTree -> SuffixTree
insert (s, n) (Node ts)
  | not hasCommonPrefix = Node ((s, Leaf n) : ts)
  | otherwise = Node (map fst ts')
  where
    ts' = map (insert' (s, n)) ts
    hasCommonPrefix = or (map snd ts')
    insert' :: (String, Int) -> Edge -> (Edge, Bool)
    insert' (s, n) (a, t)
      -- no common prefix, a, t unchanged
      | null p    = ((a, t), False)
      -- a is a prefix of s
      | null a'   = ((a, insert (s', n) t), True)
      -- p /= a
      | otherwise = ((p, Node [(s', Leaf n), (a', t)]), True)
      where
        (p, s', a') = partition s a

-- This function is given
buildTree :: String -> SuffixTree 
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

------------------------------------------------------
-- Part IV
-- data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring 
  = concat . snd . lrs

(+:) :: Num a => (a, b) -> (a, [b]) -> (a, [b])
(+:) (x, y) (x', y') = (x + x', y : y')

maximum' :: [(Int, [String])] -> (Int, [String])
maximum' [] = (0, [])
maximum' xs = maximum xs

lrs :: SuffixTree -> (Int, [String])
lrs (Leaf _)
  = (0, [])
lrs (Node xs)
  = maximum' [ (length s, s) +: (lrs (Node x)) | (s, Node x) <- xs]

------------------------------------------------------
-- Example strings and suffix trees...

s1 :: String
s1 
  = "banana"

s2 :: String
s2 
  = "mississippi"

t1 :: SuffixTree
t1 
  = Node [("banana", Leaf 0), 
          ("a", Node [("na", Node [("na", Leaf 1), 
                                   ("", Leaf 3)]), 
                     ("", Leaf 5)]), 
          ("na", Node [("na", Leaf 2), 
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2 
  = Node [("mississippi", Leaf 0), 
          ("i", Node [("ssi", Node [("ssippi", Leaf 1), 
                                    ("ppi", Leaf 4)]), 
                      ("ppi", Leaf 7), 
                      ("", Leaf 10)]), 
          ("s", Node [("si", Node [("ssippi", Leaf 2), 
                                   ("ppi", Leaf 5)]), 
                      ("i", Node [("ssippi", Leaf 3), 
                                  ("ppi", Leaf 6)])]), 
          ("p", Node [("pi", Leaf 8), 
                      ("i", Leaf 9)])]

