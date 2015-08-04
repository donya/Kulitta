> module CykParser where
> import Data.List
> import Parser

CYK Implementation
Donya Quick
Last modified: 21-July-2012



type Rule a = (a, [a])

> findProducers :: (Eq a) => [Rule a] -> [a] -> [Rule a]
> findProducers rs str = filter (\(l,r) -> r==str) rs

> cat :: [a] -> [a] -> [[a]]
> cat xs ys = [[x,y] | x<-xs, y<-ys]
 
 
> nextRow :: (Eq a) => [Rule a] -> [[[a]]] -> [[a]]
> nextRow rs rows = 
>     let n = length rows + 1 -- we assume terminal level is not included
>         segs = map (\i -> (i,n-i)) [1..n-1]
>         strs offset (i,j) = cat
>             (rows !! (i-1) !! offset)
>             (rows !! (j-1) !! (offset + i)) 
>         rules offset (i,j) = concatMap (map fst. findProducers rs) 
>                              (strs offset (i,j)) 
>         f offset = nub $ concatMap (rules offset) segs 
>     in  map f [0..length (rows !! 0) - n]


> mkSegs' :: Int -> Int -> [[Int]]
> mkSegs' n m = filter (\s -> sum s == n) $ 
>     makeRange $ take m $ repeat (0,n) where
>     makeRange = foldr (\(l,u) xs -> [(a:b) | a<-[l..u], b<-xs]) [[]]

> mkSegs :: Int -> Int -> [[Int]]
> mkSegs n m = filter (\s -> length s > 1) $ map (filter (>0)) $ mkSegs' n m

mkSegs must be called with:
n = the maximum length of the string in question.
m = the maximum possible number of substrings

n needs to be controlled by both the level (row+1) and
the offset. The formula should be:

n = min (length rows) (length (rows !! 0) - offset)

> toInds :: Int -> [Int] -> [(Int, Int)]
> toInds offset [] = []
> toInds offset (l:ls) = 
>     let row = l - 1
>         col = offset
>     in  if l <= 0 then toInds offset ls 
>         else (row, col) : toInds (offset+l) ls

> toStrs :: [[[a]]] -> [(Int, Int)] -> [[a]]
> toStrs rows [] = [[]]
> toStrs rows ((i,j):cs) = 
>     let strs = toStrs rows cs
>         theCell = if i < length rows && j < length (rows !! i)
>                  then rows !! i !! j
>                  else error ("(toStr) Bad box: ("++show i++", "++show j++")")
>     in  [(x:y) | x<-theCell, y<-strs]

> nextRowM m rs rows = -- m is the # of subdivisions
>     let n = length rows +1 -- the "level"
>         m' = min n m -- the number of substrings (or nonterms)
>         segs = mkSegs n m' -- this is ok
>         offsets = [0..length (rows !! 0) - n] -- all offsets
>         nts o s = concatMap (map fst . findProducers rs) (toStrs rows (toInds o s))
>         f o = concatMap (nts o) segs
>     in  map f offsets

> nextRowM2 m rs rows = -- m is the # of subdivisions
>     let n = length rows +1 -- the "level"
>         m' = min n m -- the number of substrings (or nonterms)
>         segs = mkSegs n m' -- this is ok
>         offsets = [0..length (rows !! 0) - n] -- all offsets
>         nts o s = concatMap (map fst . findProducers rs) (toStrs rows (toInds o s))
>         f o = concatMap (nts o) segs
>     in  map f offsets

> allRowsMS :: (Eq a) => Int -> [Rule a] -> [a] -> [[[a]]]
> allRowsMS m rs str = allRows' rs [fixRow rs $ firstRow' rs str] where
>     allRows' rs rows = if length rows == length (head rows) then rows
>                        else allRows' rs (rows ++ [fixRow rs $ nextRowM m rs rows])

> allRows :: (Eq a) => [Rule a] -> [a] -> [[[a]]]
> allRows rs str = allRows' rs [firstRow rs str] where 
>     allRows' rs rows = if length rows == length (head rows) then rows
>                        else allRows' rs (rows ++ [nextRow rs rows])


> showRows :: (Show a) => [[[a]]] -> String
> showRows rs = 
>     let f line = concatMap g line ++ "\n"
>         g bucket = show bucket ++ "\t"
>     in  concatMap f (reverse rs)

> printRows :: (Show a) => [[[a]]] -> IO ()
> printRows = putStr . showRows

==============

SYNONYM EXTENION

> findSynonyms :: (Eq a) => [Rule a] -> a -> [a]
> findSynonyms rules x = map fst $ filter (\(l,r) -> r==[x]) rules

> findSynRec :: (Eq a) => [Rule a] -> [a] -> [a]
> findSynRec rules syns = 
>     let s = nub (syns ++ concatMap (findSynonyms rules) syns)
>     in  if s == syns then syns else findSynRec rules s


 findSynRec :: (Eq a) => [Rule a] -> a -> [a]
 findSynRec rules x = 
     let s = findSynonyms rules x
         s' = nub (s ++ concatMap (findSynonyms rules) s)
     in  if s == s' then s else nub $ s ++ concatMap (findSynRec rules) s'

> fixSyns :: (Eq a) => [Rule a] -> [a] -> [a]
> fixSyns rules bucket = nub (bucket ++ concatMap (findSynonyms rules) bucket)

> fixRow :: (Eq a) => [Rule a] -> [[a]] -> [[a]]
> fixRow rules row = map (findSynRec rules) row

> allRowsS :: (Eq a) => [Rule a] -> [a] -> [[[a]]]
> allRowsS rs str = allRows' rs [fixRow rs $ firstRow rs str] where
>     allRows' rs rows = if length rows == length (head rows) then rows
>                        else allRows' rs (rows ++ [fixRow rs $ nextRow rs rows])

> firstRowOld rs cs = map (nub . map fst . findProducers rs . \a -> [a]) cs

> firstRow rs cs = map (\c -> [c]) cs

> firstRow' rs [] = []
> firstRow' rs (c:cs) = 
>     let fr0 = nub $ c : (map fst $ findProducers rs [c])
>     in  (if null fr0 then [c] else fr0) : firstRow' rs cs



==============


GENERATING ALL PARSES

 allParses :: [Rule a] -> [[[a]]] -> Int -> Int [[Rule a]]
 allParses rules rows i j = -- i is the row, j is the column
     let f :: a -> [Rule a]
         f x = filter (\(l,r) -> l==x) rules 
     in  undefined

mkSegs :: Int -> Int -> [[Int]]
mkSegs n m = filter (\s -> length s > 1) $ map (filter (>0)) $ mkSegs' n m

mkSegs must be called with:
n = the maximum length of the string in question.
m = the maximum possible number of substrings

Given a rule that we know can be applied, pick the cells it generates.

> getCells :: (Eq a) => [[[a]]] -> Rule a -> Int -> Int -> [[(Int, Int)]]
> getCells rows (lhs, rhs) level offset = 
>     let n = level + 1
>         m = length rhs -- number of syms to genererate
>         segs = mkSegs n m -- get all possible ways to chunk the string
>         inds = map (toInds offset) segs -- :: [[(Int, Int]] turn these into cells
>         -- need to filter the cells now! --
>     in  filter (goodCells rows rhs) inds

> goodCells :: (Eq a) => [[[a]]] -> [a] -> [(Int, Int)] -> Bool
> goodCells rows [] [] = True
> goodCells rows (x:xs) ((i,j):is) = 
>     elem x (rows !! i !! j) && goodCells rows xs is

> appendTo :: (Eq a) => [[[a]]] -> (Int, Int) -> a -> [[[a]]]
> appendTo [] (i,j) x = []
> appendTo xs (i,j) x = 
>     let (preRs, theRow, postRs) = (take i xs, xs !! i, drop (i+1) xs)
>         (preCs, theCell, postCs) = (take j theRow, theRow !! j, drop (j+1) theRow)
>         newCell = nub (x : theCell) -- ensure no duplicates
>     in  preRs ++ (preCs ++ newCell : postCs) : postRs

> appendTo2 :: (Eq a) => [[[a]]] -> (Int, Int) -> [a] -> [[[a]]]
> appendTo2 [] (i,j) x = []
> appendTo2 xs (i,j) x = 
>     let (preRs, theRow, postRs) = (take i xs, xs !! i, drop (i+1) xs)
>         (preCs, theCell, postCs) = (take j theRow, theRow !! j, drop (j+1) theRow)
>         newCell = nub (x ++ theCell) -- ensure no duplicates
>     in  preRs ++ (preCs ++ newCell : postCs) : postRs


The parseDown1 function completes a parse from a particular cell and symbol. We 
assume the symbol is a member of the cell in question.

> doAll = True
 
> parseDown1 :: (Eq a) => [[[a]]] -> [Rule a] -> [[[a]]] -> ((Int, Int), a) -> [[[[a]]]]
> parseDown1 rows rules newRows ((0,j),x) = [appendTo newRows (0,j) x]
> parseDown1 rows rules newRows ((i,j),x) = 
>     let newRows' = appendTo newRows (i,j) x -- put x in the current table
>         pRules = filter (\(l,r) -> l==x && length r > 0) rules -- rules of form A->BC...N
>         sRules = filter (\(l,r) -> l==x && length r == 1) rules -- rules of form A->B
>         f r = getCells rows r i j -- get a rule's target cells (CAN BE >1 LIST!)
>         f2 r@(lhs,rhs) = map (zipWith (\a (b,c) -> ((b,c),a)) rhs) (f r) -- group with coords
>         --pCells type is [[[((Int, Int), a)]]] 
>         pCells = filter (\l -> not $ null l) $ map f2 pRules -- get cells according to pRules 
>         recCall pCell = parseDown1 rows rules newRows' pCell -- recurse on rule expansions
>         pTabs = map (map (map recCall)) pCells -- gives one SET of tables per pCell
>         pTabs' = map (map combineSets) pTabs -- gives one SET of tables per inner pCell list (can be >1)
>         pTabs'' = concat $ concat pTabs' -- flatten lists, the parses are done
>         syns = filter (/=x) $ map (\(l,r) -> head r) $ sRules -- what synonym symbols are there?
>         synResults = concatMap (\a -> parseDown1 rows rules newRows' ((i,j),a)) syns -- recurse on syns
>     in  synResults ++ pTabs''


The parseDown function takes the cyk rows, the grammar's rules, and the start symbol.

> parseDown rows rules s =
>     let n = length $ head rows
>     in  map (map (map reverse)) $ parseDown1 rows rules (emptyRows n) ((n-1,0),s)

 xtrs = [(1,[1,1]), (1,[1,2]), (2,[2,2]), (3, [1]), (1, [1])] :: [Rule Int]
 xstr = [1,1,1,1] :: [Int]
 xp = allRowsMS 2 xtrs xstr
 xtest1 = parseDown xp xtrs 3

> emptyRows n = 
>     if n <= 0 then [] else take n (repeat []) : emptyRows (n-1)

2 x
1 x x
0 x x x
  0 1 2 
  
The combineSets function takes a bunch of table sets, one set per
cell that has been parsed, and finds every combination of them.
  
> combineSets :: (Eq a) => [[[[[a]]]]] -> [[[[a]]]]
> combineSets [] = error "(combineSets) No sets to combine!"
> combineSets [tset] = tset
> combineSets (tset:moreSets) = 
>     let pairs = [(a,b) | a<-tset, b<-combineSets moreSets]
>     in  map (\(a,b) -> combine1 a b) pairs
  

> -- merges N tables
> combineN :: (Eq a) => [[[[a]]]] -> [[[a]]]
> combineN [] = error "(combineN) No tables to combine!"
> combineN [t] = t
> combineN (t:ts) = combine1 t (combineN ts)

> -- merges two tables
> combine1 :: (Eq a) => [[[a]]] -> [[[a]]] -> [[[a]]]
> combine1 tab1 tab2 = 
>     let n = length $ head tab1
>         is = [(i,j) | i<-[0..n-1], j<-[0..n-1], j<=n-i-1]
>         xs = map (\(i,j) -> tab2 !! i !! j) is
>     in  foldUpdate2 tab1 (zip is xs)

> foldUpdate :: (Eq a) => [[[a]]] -> [((Int, Int), a)] -> [[[a]]]
> foldUpdate table [] = table
> foldUpdate table ((c,x):xs) = foldUpdate (appendTo table c x) xs


> foldUpdate2 :: (Eq a) => [[[a]]] -> [((Int, Int), [a])] -> [[[a]]]
> foldUpdate2 table [] = table
> foldUpdate2 table ((c,x):xs) = foldUpdate2 (appendTo2 table c x) xs

