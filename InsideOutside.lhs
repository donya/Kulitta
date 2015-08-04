Extended Inside-Outside Algorithm Implementation
Donya Quick
donyavq@netscape.net
Last modified: 01-Mar-2014

This module is an implementation of the inside-outside algorithm using 
the notations described in the following document by Michael Collins:
http://www.cs.columbia.edu/~mcollins/io.pdf

The learnProbs function will take a set of starting conditions, 
iteratively learn production probabilities, and return them. The 
qNew function will calculate a new production probability for a 
single rule.

The implementation here also contains several EXTENSIONS to the original
inside-outside algorithm, although it will perform normally given a 
CFG in Chomsky normal form. 

Modifications to original algorithm:
1. Supports rules of rank 1, A->B where A=/=B in alpha, beta, and mu
2. Supports rules of rank 3, A->BCD
3. Supports rrules of the form A->A in mu function (heuristic)
4. Supports abstract rules via an oracle to answer certain things about 
   the grammar.

The first two extensions follow directly from the equations for alpha 
and beta in the standard inside-outside algorithm. Extensions 2 will cause
a larger worst-case runtime if rules of the form A->BCD are present in the 
grammar, being O(n^4) instead of O(n^3) for the CKY/CYK parsing step. 

Extension 3 is a HEURISTIC to allow the presence of self-productions in 
the rule set. It makes the assumption that self-productions can happen but
are unlikely. In fact, it is not possible to know exactly how self-
productions should be handled without knowing the details of the generative
algorithm that produced the data set.

Extension 4 means that rules can have any form you want as long as you 
can define a function to find suitable left-hand sides given a candidate
right-hand side. The rules still need to be context-free in the sense that
you cannot account for things like AB->ACD this way. However, you can learn
parameterized grammars over infinite alphabets and rules with conditional
behavior using this approach. This is further described in my forthcoming
doctoral thesis.

The ORACLE in extension four is accounted for by the data structure TOracle.
A TOracle allows the algorithm to interact with a rule set without knowing 
its exact structure. It requires four things:

1. A list of production probabilities. It is assumed that they are ordered
   by "rule ID" or "rule index." In other words, rule 0 will have probability 
   phis !! 0.

2. The start symbol.

3. A list of "rule IDs" or "rule indices" grouped by matching left-hand sides.
   So, if we had the rules A->AA, A->B, and B->C listed in that order, then 
   the grouping would be [[0,1],[2]].
   
4. The main part of the oracle: a function to reverse rule production. If the
   rule set has the rules A->AA, A->B, and B->C, the function findByRHS should
   return the INSTANCE (TRuleInst 0 A [A,A]) when given [A,A] as input. For 
   parameterized grammars, the rule instance must have the particular parameters
   that were involved in the production. 
   
NOTE: This implementation is parallelized! To compile a program using the
learnProbs function in parallel, use:

ghc -O2 Main.lhs -rtsopts -threaded

and then run the program by:

Main [arguments] +RTS -Nx

where x is the number of threads you want. For example, if you have 2 cores, 
you will want to use -N2. If you have a quad core i7 with hyperthreading, 
you will get the best performance using -N8.

> module InsideOutside (RuleIndex, TStr, TCell, TParse, 
>                TRuleInst(TRuleInst), rule, lhs, rhs,
>                TOracle(TOracle), phis, startSym, ruleGroups, findByRHS,
>                makeAlphas, makeBetas, learnProbs, qNew,
>                uniQs, randQs, printTable) where
> import Data.List
> import CykParser -- for mkSegs' function
> import System.Random -- for random initialization of production probs
> import Control.Parallel.Strategies 

================================
CONSTANTS

> parCounts = True -- set to True to perform inner parallelization


================================
TYPE DEFINITIONS

> type RuleIndex = Int -- index into a list of rules
> type TStr a = [a] -- a string of parameterized symbols

> -- A rule "instance." There can be many instances per abstract rule.
> data TRuleInst a = TRuleInst { 
>    rule :: RuleIndex, -- index to which this instance belongs
>    lhs :: a, -- the left-hand side
>    rhs :: TStr a} -- the right-hand side
>    deriving(Eq, Show)

> type TCell a = [TRuleInst a] -- one cell in a parse table
> type TParse a = [[TCell a]] -- a full parse
> type Row = Int -- row of a parse table. Row 0 is assumed to be "terminal"-level.
> type Col = Int -- col of a parse table
> type Coord = (Row, Col) -- a particular point in a table, corresponding to one cell.

> -- A learning "environment:" acts as an oracle to describe the grammar
> data TOracle a = TOracle {
>     phis :: [Double], -- production probabilities sorted by rule index
>     startSym :: a, -- start symbol
>     ruleGroups :: [[RuleIndex]], -- which rule indexes have the same left-hand side?
>     findByRHS :: [a] -> [TRuleInst a]} -- which rules have a particular right-hand side?

> type Alpha a = ((a, Int, Int), Double) -- a single alpha value tagged with its symbol and span
> type Beta a = ((a, Int, Int), Double) -- a single beta value tagged with its symbol and span

Controlled product function that stops when a zero is encountered to 
avoid evaluating unnecessary terms. The prod function will terminate on
on infinite list if it contains a zero, whereas product will not.

> prod :: (Num a, Eq a) => [a] -> a
> prod [] = error "(prod) Nothing to multiply."
> prod [x] = x
> prod (x:xs) = if x==0 then 0 else x * prod xs

Conversions in an out of row/column vs. span representation to 
determine what span a cell in a parse table has and visa versa.

> toRC, toIJ :: (Int,Int) -> (Int, Int)
> toRC (i, j) = (j-i, i)
> toIJ (row,col) = (col,col+row)

For backward compatibility:

> getParse :: (Eq a) => TOracle a -> TStr a -> TParse a
> getParse = buildTable 

> idRule :: (Eq a) => TRuleInst a -> Bool
> idRule (TRuleInst id lhs [rhs]) = rhs==lhs
> idRule _ = False

===========
ALPHA CALC

Symbol enumeration function. Symbols are ordered for most efficient
computation of alpha values.

> symEnum :: (Eq a) => TOracle a -> TStr a -> [(a, Int, Int)]
> symEnum env xs = 
>     let p = getParse env xs -- get the parse of the string
>         n = length xs
>         f (r,c) =  map (toAIJ (r,c)) $ p !! r !! c -- synonyms are originally stacked higher to left
>         rcs = [(r,c) | r<-[0..n-1], c<-[0..n-1], r+c<n]
>     in  concatMap f rcs where
>     toAIJ :: Coord -> TRuleInst a -> (a, Int, Int)
>     toAIJ coord t = 
>         let (i,j) = toIJ coord
>         in  (lhs t, i, j)

Function for initial generation of alpha values. Values that are 
calculated are then stored in a list for use in recursive calls.

> makeAlphas :: (Eq a) => TOracle a -> TStr a -> [Alpha a]
> makeAlphas env xs = 
>     let syms = symEnum env xs -- (symEnum env) xs 
>     in filter ((>0).snd) $ makeRec env [] xs syms where
>     makeRec env as xs [] = as
>     makeRec env as xs (v@(a,i,j):vs) = 
>        let aNew = alpha env as xs a i j 
>        in  makeRec env ((v,aNew):as) xs vs

Helper function to determine what a symbol can produce.

> traceDown :: (Eq a) => TOracle a -> TStr a -> a -> Int -> Int -> [TRuleInst a]
> traceDown env xs a i j = 
>     let p = getParse env xs
>         (row,col) = toRC (i, j)
>     in  filter (\r -> lhs r == a) $ p !! row !! col

Generating a single alpha value.

> alpha :: (Eq a) => TOracle a -> [Alpha a] -> TStr a -> a -> Int -> Int -> Double
> alpha env as xs a i j = case (lookup (a,i,j) as) of 
>     Just v -> v
>     Nothing -> 
>         let rInsts = filter (not.idRule) $ -- cases of A->A are not allowed (but A->B is ok)
>                      traceDown env xs a i j -- get possible children of A as rule instances
>             subAs = sum $ map (alphaRec env as xs i j) rInsts
>         in  if i==j && a == (xs !! i) then 1.0 -- check for found terminal
>             else if null rInsts then 0.0 -- check for dead end on wrong terminal
>             else subAs where -- recursive case

> alphaRec :: (Eq a) => TOracle a -> [Alpha a] -> TStr a -> Int -> Int -> TRuleInst a -> Double
> alphaRec env as xs i j (TRuleInst id lhs [rhs]) = 
>     prod [phis env !! id, alpha env as xs rhs i j]
> alphaRec env as xs i j (TRuleInst id lhs [s1, s2]) = 
>     let ks = [i..j-1]
>         f k = prod [alpha env as xs s1 i k, alpha env as xs s2 (k+1) j]
>     in  if i == j then 0.0 -- dead end, no way to split 
>         else prod [phis env !! id, sum $ map f ks] -- phi multiplication moved here for efficiency
> alphaRec env as xs i j (TRuleInst id lhs [s1, s2, s3]) = 
>     let ks = [(k,l) | k<-[i..j-2], l<-[i+1..j-1], k<l]
>         f (k,l) = prod [alpha env as xs s1 i k, alpha env as xs s2 (k+1) l, alpha env as xs s3 (l+1) j]
>     in  if j - i < 2 then 0.0 -- dead end, no way to split 
>         else prod [phis env !! id, sum $ map f ks]
> alphaRec env as xs i j tr = error 
>     ("(alphaRec) Unable to handle rule of rank "++show (length $ rhs tr))


Function to lookup a value after they've been calculated. Only values >0 are stored 
to avoid storing lots of garbage values.

> alpha' :: (Eq a) => [Alpha a] -> a -> Int -> Int -> Double
> alpha' as a i j = case (lookup (a,i,j) as) of
>     Just v -> v
>     Nothing -> 0.0

===========
BETA CALC

Function for initial generation of beta values.  Values that are 
calculated are then stored in a list for use in recursive calls.

> makeBetas :: (Eq a) => TOracle a -> [Alpha a] -> TStr a -> [Beta a]
> makeBetas env as xs = 
>     let syms = reverse $ symEnum env xs -- reverse of alpha's order
>     in  filter ((>0).snd) $ makeRec' env [] as xs syms where
>     makeRec' env bs as xs [] = bs
>     makeRec' env bs as xs (v@(a,i,j):vs) = 
>        let bNew = beta env bs as xs a i j 
>        in  makeRec' env ((v,bNew):bs) as xs vs

Helper function to determine what rule instances could have produced a 
particular symbol.

> traceUp :: (Eq a) => TOracle a -> TStr a -> a -> Int -> Int -> [TRuleInst a]
> traceUp env xs a i j = 
>     let p = getParse env xs
>         n = length xs
>         otherSpans = [toRC (i, j) | i<-[0..i], j<-[j..n-1]]
>         cands = concatMap (\(r',c') -> p !! r' !! c') otherSpans
>     in  nub $ filter (\ru -> elem a (rhs ru)) $ cands

Calculate a single beta value.

> beta :: (Eq a) => TOracle a -> [Beta a] -> [Alpha a] -> TStr a -> a -> Int -> Int -> Double
> beta env bs as xs a i j = case (lookup (a,i,j) bs) of 
>     Just v -> v
>     Nothing -> if alpha' as a i j <= 0 then 0.0 else -- added for optimization purposes
>         let rInsts = filter (not.idRule) $ -- cases of A->A are not allowed (but A->B is ok)
>                      traceUp env xs a i j -- get possible parents of A as rule instances
>             subBs = sum $ map (betaRec env bs as xs a i j) rInsts
>         in  if i==0 && j==(length xs - 1) && a == startSym env then 1.0 -- check for found terminal
>             else if null rInsts then 0.0 -- check for dead end on wrong terminal
>             else subBs where -- recursive case            

Recursive beta calculation.

> betaRec :: (Eq a) => TOracle a -> [Beta a] -> [Alpha a] -> TStr a -> 
>            a -> Int -> Int -> TRuleInst a -> Double
> betaRec env bs as xs a i j (TRuleInst id lhs [rhs]) = 
>     prod [phis env !! id, beta env bs as xs lhs i j] -- travel up to a synonym
> betaRec env bs as xs a i j (TRuleInst id lhs [s1, s2]) = -- a is s1, s2, or both 
>     let ks1 = [j+1..length xs-1] -- for B -> AC
>         ks2 = [0..i-1] -- for B -> CA
>         f1 k = prod [beta env bs as xs lhs i k, alpha' as s2 (j+1) k] -- for B -> AC
>         f2 k = prod [beta env bs as xs lhs k j, alpha' as s1 k (i-1)] -- for B -> CA
>         parts = sum $ map snd $filter (fst) $ zip [a==s1, a==s2] 
>                 [sum $ map f1 ks1, sum $ map f2 ks2]
>     in  prod [phis env !! id, parts]
> betaRec env bs as xs a i j (TRuleInst id lhs [s1, s2, s3]) = 
>     let ks1 = [(k,l) | k<-[j+1..length xs-2], 
>                l<-[j+2..length xs-1], k<l] -- for B -> ACD
>         f1 (k,l) = prod [beta env bs as xs lhs i l, alpha' as s2 (j+1) k, 
>                          alpha' as s3 (k+1) l] -- for B -> ACD
>         ks2 = [(k,l) | k<-[0..i-1], l<-[j+1..length xs-1]] -- for B -> CAD
>         f2 (k,l) = prod [beta env bs as xs lhs k l, alpha' as s1 k (i-1), 
>                          alpha' as s3 (j+1) l] -- for B -> CAD
>         ks3 = [(k,l) | k<-[0..i-2], l<-[1..i-1], k<l] -- for B -> CDA
>         f3 (k,l) = prod [beta env bs as xs lhs k j, alpha' as s1 k (l-1), 
>                          alpha' as s2 l (i-1)] -- for B -> CDA
>         parts = sum $ map snd $filter (fst) $ zip [a==s1, a==s2, a==s3] 
>                 [sum $ map f1 ks1, sum $ map f2 ks2, sum $ map f3 ks3]
>     in  prod [phis env !! id, parts]
> betaRec env bs as xs a i j tr = error 
>     ("(betaec) Unable to handle rule of rank "++show (length $ rhs tr))

Function for lookup of calculated beta values. Only non-zero values are stored.

> beta' :: (Eq a) => [Beta a] -> a -> Int -> Int -> Double
> beta' bs a i j = case (lookup (a,i,j) bs) of
>     Just v -> v
>     Nothing -> 0.0


===========
MU CALC

Calculate mu for a particular rule with a particular span.
NOTE: mu allows for ONE instance of A->A in the parse tree. This is a heuristic
to account for the presence of such rules but avoids over-abundance (otherwise 
they could occur infinitely often).

> mu :: (Eq a) => TOracle a -> [Alpha a] -> [Beta a] -> TStr a -> 
>                       TRuleInst a -> [Int] -> Double
> mu env as bs xs (TRuleInst id a [b]) [i,j] = prod[phis env !! id, beta' bs a i j, alpha' as b i j]
> mu env as bs xs (TRuleInst id a [b,c]) [i,k,j] = 
>     prod[phis env !! id, beta' bs a i j, alpha' as b i k, alpha' as c (k+1) j]
> mu env as bs xs (TRuleInst id a [b,c,d]) [i,k,l,j] = 
>     prod[phis env !! id, beta' bs a i j, alpha' as b i k, alpha' as c (k+1) l, alpha' as d (l+1) j]
> mu env as bs xs _ _ = error "(mu) Bad TRuleInst or index list."

Calculate z, the probability of the entire tree for normalization.

> z :: (Eq a) => TOracle a -> [Alpha a] -> TStr a -> Double
> z env as xs = alpha' as (startSym env) 0 (length xs - 1)


===========
NEW Q CALC

Count algorithm implementation that uses the parse tree for efficiency.

> count env as bs xs rID = 
>     let p = getParse env xs 
>         n = length xs
>         f (i,j) = map (\r -> (r,i,j)) $ p !! i !! j -- get all rules out of a cell
>         coords = [(i,j) | i<-[0..n-1], j<-[0..n-1], i+j<n]  -- find cells with the right span
>         -- find only rules with the right ID (and span)
>         insts = map toIJs $ filter (\(r,i,j) -> rule r == rID) $ concatMap f coords 
>         result = sum (map (countB env as bs xs) insts) / denom
>         denom = z env as xs
>     in  if denom<=0 then error "(count) z value of <=0" else result where
>     toIJs (r, row,col) = (r, col, col+row)

> countB :: (Eq a) => TOracle a -> [Alpha a] -> [Beta a] -> TStr a -> (TRuleInst a, Int, Int) -> Double
> countB env as bs xs (r@(TRuleInst id a [b]), i, j) = sum $ map (mu env as bs xs r) [[i,j]]
> countB env as bs xs (r@(TRuleInst id a [b,c]), i, j) = sum $ 
>     map (mu env as bs xs r) [[i,k,j] | k<-[i..j-1]]
> countB env as bs xs (r@(TRuleInst id a [b,c,d]), i, j) = sum $ 
>     map (mu env as bs xs r) [[i,k,l,j] | k<-[i..j-2], l<-[i+1..j-1], k<l, l<j]
> countB env as bs xs _ = error "(countB) Bad TRuleInst or mismatched index list."

Sum counts over all strings in the data set. If usePar==True, then
the parallelized version is used.

> f usePar env triples rID = 
>     let exprs = map (\(xs,as,bs) -> count env as bs xs rID) triples
>     in  if usePar then sum $ seq (triples) (exprs `using` parList rdeepseq)
>         else sum $ exprs

Find the normalization factor for a given rule.

> qNorm :: (Eq a) => TOracle a -> [(TStr a, [Alpha a], [Beta a])] -> RuleIndex -> Double
> qNorm env triples rID = sum $ map (f parCounts env triples) $ 
>                         findGroup (ruleGroups env) rID where
>     findGroup [] rID = error ("(qNorm) Uknown rule: "++show rID)
>     findGroup (g:gs) rID = if elem rID g then g else findGroup gs rID

Re-estimate the probability of a rule. It is possible that a rule will not occurr 
anywhere in the data set, and that can cause a zero denominator that would give 
NaN as a result instead of 0.0, hence the check on the denominator below. 

> qNew :: (Eq a) => TOracle a -> [(TStr a, [Alpha a], [Beta a])] -> RuleIndex -> Double
> qNew env triples rID = 
>     let denom = qNorm env triples rID -- if this is zero then the rule hasn't occurred
>         result = f parCounts env triples rID / denom
>     in  if denom <= 0 then 0.0 else result 

Iteratively compute the production probabilities over a data set given a 
maximum number of iterations to run, aminimum probability (in case a rule 
doesn't appear) and a threshold of change between two iterations' 
distributions under which to stop.

NOTE: this version does NOT check for cycles, such as a rule set that has
both A->B and B->A. Rule sets with cycles will spin infinitely.

> learnProbs :: (Eq a, Show a) => TOracle a -> [TStr a] -> Int -> Double -> Double -> IO [[Double]]
> learnProbs env strs i minP dThresh = if i<=0 then return [] else do
>     let as = map (makeAlphas env) strs
>         bs = zipWith (\a s -> makeBetas env a s) as strs
>         xab = zip3 strs as bs
>     putStrLn "Calculating production probs..."
>     let qs0 = phis env -- starting probabilities
>         qs1s = map (qNew env xab) [0..length qs0 - 1] -- new probabilities
>         qs1 = seq (as, bs, env) (qs1s `using` parList rdeepseq) -- parallelize computation of qs1
>         qf' =  if minP > 0.0 then normalize2 (ruleGroups env) minP qs1 -- adjust minimum probs
>                else normalize (ruleGroups env) qs1
>         env' = env{phis = qf'} -- environment for the next iteration (updated probs)
>         diff = totalDiff qs0 qf' -- total change in probability mass
>     putStrLn ("Total probability mass changed: "++show diff)
>     putStrLn $ concat $ intersperse ", " $ map show qf' 
>     let badInds = findIndices (\x -> x<=0) qs1 -- are there any rules with prob <= 0?
>         badStr = if null badInds then "ok" -- all are ok
>                  else "Zero-prob rules: "++show badInds -- some rules are prob <=0
>         (iNext, skipping) = if diff <= dThresh then (0, True) -- check for convergence
>                             else (i-1, False) 
>         itrStr = if skipping then ("Q has converged with "++show (i-1)++" iterations left.")
>                  else ("Iterations left: "++show (i-1))
>     putStrLn itrStr
>     qNext <- learnProbs env' strs iNext minP dThresh -- compute next iteration
>     return (qf' : qNext) -- return all iterations' probability distributions


Helper function to correct probabilities that are too small. It performs
normalization at the same time.

> normalize2 :: [[Int]] -> Double -> [Double] -> [Double]
> normalize2 gs minP qs = 
>     let gqs = map (map (qs !!)) gs -- collect probabilities by rule group
>         gqs' = map (fixMins minP) gqs -- correct for non-zero values
>         gqsi = zip (concat gs) (concat gqs') -- flatten the list
>     in  map snd $ sort gqsi -- place the list back in its original order
 
> fixMins minP xs = 
>     let isOk = findIndices (>=minP) xs 
>         isZ = findIndices (<minP) xs 
>         okVals = map (xs !!) isOk -- collect all of the values that are ok
>         newSum = sum okVals + (fromIntegral (length isZ) * minP) -- adjust the normalization factor
>     in  map (\x -> if x<=0 then minP else x/newSum) xs -- normalize everything

Produce uniform initial probabilities.

> uniQs :: [[RuleIndex]] -> [Double]
> uniQs gs = normalize gs $ take (length $ concat gs) $ repeat 1.0

Produce random initial probabilities.

> randQs :: [[RuleIndex]] -> StdGen -> [Double]
> randQs gs g = normalize gs $ take (length $ concat gs) $ vals g where
>     vals g = let (d,g') = randomR (0.0, 1.0) g in d : vals g'

Normalization function for production probabilities.

> normalize :: [[RuleIndex]] -> [Double] -> [Double]
> normalize gs qs = concatMap (\g -> map (/sum g) g) $ map (map (qs !!)) gs

Compute the total difference in two distributions of production probabilities.
The result is the total change in probability mass (NOT a percentage). 

> totalDiff :: [Double] -> [Double] -> Double
> totalDiff d1 d2 = sum $ map abs $ zipWith subtract d1 d2 


============================

PARAM-BASED PARSING

The following is an implementation of standard CKY/CYK parsing
with extensions to allow rules of the form A->BCD, A->B, and 
self productions of the form A->A. It uses the same oracle approach
as the code above to parse using rule instances rather than the
rule set itself.

The updateTable function will take a partially filled parse table and 
update the particular cell in question with any rules that will fit.

> blankTable :: Int -> TParse a
> blankTable i = if i<=0 then [] else 
>     take i (repeat []) : blankTable (i-1)

The startTable function builds an initial table. The rest is blank.

> startTable :: TStr a -> TParse a 
> startTable xs = 
>     let row0 = map (\x -> [TRuleInst (-1) x []]) xs
>     in   row0 : blankTable (length xs - 1)

Given an environment and a string, buid a parse table.

> buildTable :: (Eq a) => TOracle a -> TStr a -> TParse a
> buildTable env xs = 
>     let st = startTable xs
>         n = length xs
>         rowCols = [(r,c) | r<-[0..n-1], c<-[0..n-1], c<=n-(r+1)]
>     in  foldl' (\pt rc -> updateTable' env pt xs rc) st rowCols

Iteratively update the table. Individual cells must be checked at least
twice each to ensure that all synonyms get added.

> updateTable' env pTable xs (row,col) = 
>     let pTable' = updateTable env pTable xs (row,col)
>     in  if pTable == pTable' then pTable else updateTable' env pTable' xs (row, col)

> updateTable :: (Eq a) => TOracle a -> TParse a -> TStr a -> Coord -> TParse a
> updateTable env pTable xs (row,col) = 
>     let cellPats = getCellCombos (row,col) 3 :: [[Coord]]
>         cellStrs = concatMap (toStr pTable) cellPats -- :: [TStr a]
>         newRules = concatMap (findByRHS env) cellStrs -- :: [TRuleInst a]
>     in  updateCell pTable (row,col) newRules 

Function to update an individual cell in a table with a new value.

> updateCell :: (Eq a) => TParse a -> (Int, Col) -> [TRuleInst a] -> TParse a
> updateCell pTable (row,col) v = 
>     let preRows = take row pTable
>         postRows = drop (row+1) pTable
>         preCols = take col (pTable !! row)
>         postCols = drop (col+1) (pTable !! row)
>         newVal = nub (v ++ (pTable !! row !! col)) -- concat vals onto existing cell
>     in  preRows ++ [preCols ++ [newVal] ++ postCols] ++ postRows


Given a starting cell and a maximum number of segments to which it can 
belong (this will be the rank of the grammar), return a series of other
coordinates to investigate. The starting cell is presumed to be the 
"generator" of the other segments.

> getCellCombos :: Coord -> Int -> [[Coord]]
> getCellCombos (row,col) maxSegs = if maxSegs <=0 then [] else
>     let symCount = row+1 -- total number of symbols
>         maxSegs' = min symCount maxSegs -- maximum number of segments
>         segs = mkSegs' symCount maxSegs' -- symbol divisions 
>     in  nub $ map (toInds col) segs

Turn a series of coordinates into a string of symbols. For grammars with a 
terminal/nonterminal division, this will be a potentially mixed string of 
terminals and nonterminals.

> toStr :: TParse a -> [Coord] -> [TStr a]
> toStr pTable coords = 
>     let f (r,c) = if r < length pTable && c < length (pTable !! r) 
>                   then pTable !! r !! c else error ("(toStr) Bad coord: "++show (r,c))
>         cells = allCombos $ map f coords
>     in  map (map lhs) cells

Given a set of collections of items (e:es), find all combinations 
with one item from each collection in sequence.

> allCombos :: [[a]] -> [[a]]
> allCombos [] = [[]]
> allCombos (e:es) = [(e':es') | e'<-e, es'<-allCombos es]

Display functions for the parts of a parse table. When printing the
parse table, although rules are stored in each cell, only the lefthand
side of each rule will be printed to correspond to the more standard 
CKY/CYK representation.

> showCell :: (Show a) => TCell a -> String
> showCell trs = show $ map lhs trs

> showRow :: (Show a) => [TCell a] -> String
> showRow trs = concat (intersperse "  " $ map showCell trs)

> showTable :: (Show a) => TParse a -> String
> showTable rs = concat $ intersperse "\n" $ map showRow rs

> printTable ::(Show a) => TParse a -> IO()
> printTable = putStrLn . showTable
