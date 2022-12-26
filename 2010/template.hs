data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
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
-- data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 

getIndices :: SuffixTree -> [Int]
getIndices (Leaf x)
  = [x]
getIndices (Node xs)
  = concatMap getIndices (map snd xs)

partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition 
  = undefined

findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings'
  = undefined

------------------------------------------------------

insert :: (String, Int) -> SuffixTree -> SuffixTree
insert 
  = undefined

-- This function is given
buildTree :: String -> SuffixTree 
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

------------------------------------------------------
-- Part IV

longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring 
  = undefined

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

