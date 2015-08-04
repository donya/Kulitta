CYK Parsing Module
Donya Quick

Last modified: 19-Dec-2014

> module Parser where
> import Data.List

> type Rule a = (a, [a])
> type Partial a = [Rule a]
> type Parse a = [Partial a]

1. Given a list of symbols, need a way to find
all possible rule parses. 

The following function will find possible rules to apply
to the start of a string (list) of symbols.

> findMatches :: (Eq a) => [Rule a] -> [a] -> [(a, [a])]
> findMatches rules xs = 
>     let f (lhs, rhs) = take (length rhs) xs == rhs
>     in  filter f rules


We try brute-force left to right parsing, given a rule list.

> recParse1 :: (Eq a) => [Rule a] -> [a] -> [Partial a] -- [[(a, [a])]]
> recParse1 rules [] = [[]]
> recParse1 rules xs = 
>     let mats = findMatches rules xs -- get all possible next steps
>         f (lhs, rhs) = drop (length rhs) xs 
>         xs' = map f mats -- cut xs based on mats
>         finals = map (recParse1 rules) xs'
>         finals' = zipWith (\h ts -> map (h:) ts) mats finals
>     in  filter (okParse xs) $ concat finals'

if null mats then [] else undefined -- filter (okParse xs) finalStrs

> okParse :: (Eq a) => [a] -> [(a, [a])] -> Bool
> okParse xs parse = length xs == length (concatMap snd parse)


> nextLevel :: (Eq a) => [Rule a] -> Partial a -> [Partial a]
> nextLevel rules = recParse1 rules . map fst 


We need to do the following:
1. First form an initial parse list. Must go from [a] to [Partial a].
2. For each Partial, x, create a new [Partial a], xs. 
	- for each y in xs, append x to it. Make it a Parse a.
	
	
> iterParseStep :: (Eq a) => [a] -> [Rule a] -> Parse a -> [Parse a]
> iterParseStep dset rules [] = map (\x -> [x]) $ recParse1 rules dset
> iterParseStep dset rules parse = 
>     let theStr = head parse
>         nextLevels = nextLevel rules theStr
>     in  map (\x -> x:parse) nextLevels

> isStart :: (Eq a) => a -> [Rule a] -> Bool
> isStart ssym [(a,bs)] = ssym==a
> isStart ssym _ = False

> type StopFun a = Parse a -> Bool

> noReps :: (Eq a) => [a] -> Bool
> noReps [] = True
> noReps (x:xs) = not (elem x xs) && noReps xs

> isNew :: (Eq a) => [[a]] -> [a] -> Bool
> isNew allPs newP = 
>     let x = head newP
>         f p = elem x p
>     in  not $ or $ map f allPs

> filterUniques :: (Eq a) => [[a]] -> [[a]]
> filterUniques [] = []
> filterUniques (x:xs) = 
>     if isNew xs x then x:filterUniques xs else filterUniques xs

> removeRedundants :: (Eq a) => [[a]] -> [[a]]
> removeRedundants xs = 
>     let f a bs = elem (head a) $ tail bs
>         g x = not $ or $ map (f x) xs
>     in  filter g xs

> iterParse :: (Eq a) => StopFun a -> [a] -> [Rule a] -> [Parse a] -> Int -> Int -> [Parse a]
> iterParse stopFun dset rules parses count lim = 
>     let iStops = findIndices stopFun parses -- find any finished parses
>         parses' = concatMap (iterParseStep dset rules) parses
>         fullParse = iterParse stopFun dset rules (iterParseStep dset rules []) (count+1) lim
>         recParse = filter (noReps) $ nub $ iterParse stopFun dset rules parses' (count+1) lim
>         recParse' = filterUniques recParse
>     in  if count >= lim then parses else -- stop because of iterations limit
>         if null parses then fullParse else -- start from beginning
>         if null iStops then recParse' else map (parses !!) iStops

> parse :: (Eq a) => StopFun a -> [Rule a] -> [a] -> Int -> [Parse a]
> parse stopFun rules dset maxIters = iterParse stopFun dset rules [] 0 maxIters


> iterParse2 :: (Eq a) => StopFun a -> [a] -> [Rule a] -> [Parse a] -> Int -> Int -> [Parse a]
> iterParse2 stopFun dset rules parses count lim = 
>     let iStops = findIndices stopFun parses -- find any finished parses
>         fParses = map (parses!!) iStops
>         uParses = map (parses!!) [x | x<-[0..length parses-1], not $ elem x iStops]
>         parses' = concatMap (iterParseStep dset rules) uParses
>         fullParse = iterParse2 stopFun dset rules (iterParseStep dset rules []) (count+1) lim
>         recParse = nub $ iterParse2 stopFun dset rules parses' (count+1) lim
>     in  if null parses then fullParse else 
>         if count >= lim then fParses else (fParses ++ recParse)

> parseAll :: (Eq a) => StopFun a -> [Rule a] -> [a] -> Int -> [Parse a]
> parseAll stopFun rules dset maxIters = iterParse2 stopFun dset rules [] 0 maxIters

======= DISPLAY =======

> showRHS :: (Show a) => Partial a -> String
> showRHS = show . concatMap snd

> showParse :: (Show a) => Parse a -> String
> showParse [] = []
> showParse [x] = showLevel x ++ "\n" ++ showRHS x ++ "\n\n"
> showParse (h:t) = showLevel h ++ "\n" ++ showParse t

> showLevel :: (Show a) => Partial a -> String
> showLevel [] = []
> showLevel (h:t) = show h ++ " " ++ showLevel t


> printParse p = putStr $ showParse p

======= TESTING =======

> testStr = [1,1,1]
> testRules = [(1, [1,1]),
>              (1, [1,2]),
>              (2, [2,2]),
>              (1, [1])]

> testStop :: StopFun Int
> testStop ([(1,_)]:_) = True
> testStop _ = False

> testP = parse testStop testRules testStr 100
> testP' = parseAll testStop testRules testStr 100

> testStr2 = [1,1,1,2]
> testP2 = parse testStop testRules testStr2 100
> testP2' = parseAll testStop testRules testStr2 100