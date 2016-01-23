Search algoritm for use with chord spaces
Donya Quick and Paul Hudak
Last modified: 13-Jan-2016

Implementation of a search algorithm for traversing chord spaces using
let-in constraints as well as progression level predicates.


> module Kulitta.Search where
> import Data.List
> import Kulitta.ChordSpaces
> import Control.DeepSeq
> import Control.Parallel.Strategies
> import System.Random
> import Kulitta.PTGG hiding (choose)
> import Kulitta.Grammars.MusicGrammars
> import Kulitta.PostProc
> import Kulitta.Constraints

> type Constraints = [[(Int, Int)]]
> type Index = Int
> type Bound = Int

> printIt = True -- flag for whether to show evidence of search progress
> printIter = 5000 -- number of solutions to check before reporting back

========= OBTAINING SOLUTIONS =========

The allSolns function finds all R-equivalent sequences given a quotient
space, qs, and a relation, r.

This is the obvious, but slightly less efficient way to obtain solutions.
The lazy evaluation in Haskell allows it to do aggressive pruning similarly
to how pairProg handles it.

> allSolns :: (Eq a, Show a) => QSpace a -> EqRel a -> [a] -> [[a]]
> allSolns qs r [] = [[]]
> allSolns qs r (x:xs) = 
>     [(y:ys) | y <- eqClass qs r x, ys <- allSolns qs r xs]

Unfortunately, allSolns won't work with really big problems. Large chord 
spaces and long progressions (even with small chord spaces) cause an 
exponential blowup in the search space. Two alternative options below
prune the solution space more aggressively.

The pairProg function applies a predicate as it generates the 
solutions.

> pairProg :: (Eq a, Show a) => QSpace a -> EqRel a -> Predicate (a,a) -> [a] -> [[a]]
> pairProg qs r c [] = []
> pairProg qs r c [x] = map (\a -> [a]) $ eqClass qs r x
> pairProg qs r c (x:xs) = 
>     let endSolns = pairProg qs r c xs -- map xs to a new progression
>         f y soln = c (y, head soln) -- filter using the classifier
>         newSolns = [(y:ys) | y<-eqClass qs r x, ys<-endSolns, f y ys]
>     in  if null newSolns then error "No solutions that satisfy the consraints!"
>         else newSolns

Now we define the greedy algorithm, greedyProg.

We define choose, a function to stochastically select an element
from a list.

> choose :: StdGen -> [a] -> (StdGen, a)
> choose g xs = 
>     let (r, g') = next g
>     in  (g', xs !! (r `mod` length xs))

And finally the recursive, greedy function, greedyProg, and its
"helper," greedyChord.


> type Fallback a = EqClass a -> StdGen -> a -> (StdGen, a)
		 	 
> greedyProg :: (Eq a, Show a) => QSpace a -> EqRel a -> 
>     Predicate (a,a) -> Fallback a -> StdGen -> [a] -> [a]
> greedyProg qs r c f g [] = []
> greedyProg qs r c f g (x:xs) =
>     let e = eqClass qs r x
>         (g', y0) = choose g e -- randomly choose the first AbsChord
>     in  greedyRec qs r c f g y0 xs where 
>         greedyRec qs r c f g y pts = 
>             let (g', yi) = greedyChord (eqClass qs r (head pts)) y c f g
>             in  if null pts then [y] 
>                 else y : greedyRec qs r c f g' yi (tail pts)

> greedyChord :: (Eq a, Show a) => EqClass a -> a -> Predicate (a,a) -> 
>     Fallback a -> StdGen -> (StdGen, a)
> greedyChord e yprev hpair f g = 
>     let (rand, g') = next g
>         yxs = zip (repeat yprev) e
>         ys = map snd $ filter hpair yxs
>     in  if null ys then f e g' yprev -- fallback case
>         else (g', ys !! (rand `mod` length ys)) -- ok case


Now we need some fall-back functions. We can construct them from
predicates.

> fallFun :: (Show a) => Predicate (a,a) -> EqClass a -> StdGen -> a -> (StdGen, a)
> fallFun c e g x = 
>     let ys = map snd $ filter c $ zip (repeat x) e
>         (i, g') = next g
>     in  if null ys then error ("Stuck at symbol "++show x++". No viable options left!")
>         else (g', ys !! (i `mod` length ys))

This is the default fallback function we used in our experiments:

> defFall = fallFun (maxClass 7)


And also a nearest neighbor fallback function that would always 
succeed if the equivalence class has at least one element (which 
should always be the case).

> nearFall ::  EqClass AbsChord -> StdGen -> AbsChord -> (StdGen, AbsChord)
> nearFall e g x = 
>     let ds = map (simpleDist x) e :: [Double]
>         y = e !! (head $ findIndices (==minimum ds) ds)
>     in  (g, y)


This version of greedyProg operates over a list of equivalence classes.

> greedyProg' :: (Eq a, Show a) => 
>     Predicate (a,a) -> Fallback a -> StdGen -> [EqClass a] -> [a]
> greedyProg' c f g [] = []
> greedyProg' c f g (e:es) =
>     let (g', y0) = choose g e -- randomly choose the first AbsChord
>     in  greedyRec' c f g y0 es where 
>         greedyRec' c f g y pts = 
>             let (g', yi) = greedyChord (head pts) y c f g
>             in  if null pts then [y] 
>                 else y : greedyRec' c f g' yi (tail pts)

> pairProg' :: (Eq a, Show a) => Predicate (a,a) -> [EqClass a] -> [[a]]
> pairProg' c [] = [[]]
> pairProg' c (e:es) = 
>     let newSolns = [(y:ys) | y<-e, ys<-pairProg' c es, c (y, head ys)]
>     in  if not $ null newSolns then newSolns
>         else error "No solutions that satisfy the consraints!"


======================

LET CONSTRAINTS

Let-in expressions impose constraints on the interpretation of
the results as music. These can be viewed as progression-level
constraints, or predicates.

mkCons fundtion looks through the let-in structure of a Term
and produces a progression-level predicate for use with the
ChordSpaces module.

> mkCons :: (Eq a) => [Term b c] -> Predicate [a]
> mkCons t xs = toCons (findInds [] t) xs where
>     toCons :: (Eq a) => [[(Int, Int)]] -> [a] -> Bool
>     toCons [] xs = True
>     toCons (c:cs) xs = 
>         let f (i,j) = take (j+1-i) $ drop i xs 
>         in  (and $ map (f (head c) == ) $ 
>             map f $ tail c) && toCons cs xs


The findInds function looks through a Term for let-in expressions.
When it finds Let x a exp, it calls findIndsSub on x and exp to 
determine which indices in the sequence are occupied by instances
of x. 

> findInds :: [(String, [Term a b])] -> [Term a b] -> [[(Int, Int)]]
> findInds e [] = []
> findInds e (t:ts) = let rest = findInds e ts in case t of 
>     Var x -> undefined
>     NT x -> map (map (pAdd 1)) $ findInds e ts
>     Let x a exp -> 
>         let a' = expand e a
>             exp' = expand ((x,a'):e) exp
>         in  findInds e a ++ findIndsSub x (length a') (expand' e exp) : 
>             map (map (pAdd $ length exp')) rest

> expand' :: [(String, Sentence a b)] -> Sentence a b -> Sentence a b
> expand' e [] = []
> expand' e (t:ts) = case t of 
>     Let x a exp -> expand' ((x, expand' e a) : e) exp ++ expand' e ts
>     Var x       -> case lookup x e of 
>                        Nothing -> Var x : expand' e ts
>                        Just a -> a ++ expand' e ts
>     x           -> x : expand' e ts


The findIndsSub function looks for instances of a variable and
determines what indices they occupy.

> findIndsSub :: String -> Int -> [Term a b] -> [(Int, Int)]
> findIndsSub x xLen [] = []
> findIndsSub x xLen (t:ts) = let rest = findIndsSub x xLen ts in case t of
>     Var y -> if x==y then (0, xLen-1) : map (pAdd xLen) rest else rest
>     NT y  -> map (pAdd 1) $ findIndsSub x xLen ts
>     Let y a e -> error "(find Instances) This point should be unreachable."

> pAdd amt (a,b) = (a+amt, b+amt)

========================

SEARCH IMPLEMENTATION


All of these functions assume that constraints are SORTED. This is used to  
"jump ahead" to the next solution that would meet constraints early-on in the 
depth-first traversal. We give preference to changing indices further right 
in the sequence. It is assumed that the constraints are fully sorted (inner 
and outer) by doing the following for k :: Constraints:

	sort (map sort k)
	
If this is not done, then the constraints cannot be guaranteed to be satisfied.
The findSoln function looks for the first solution satisfying two types of 
constraints: those from let-in statements and more generic, progression-level
constraints supplied as a Predicate function.

> findSoln :: (Eq a, Show a) => 
>     Constraints -> Predicate [a] -> [[a]] -> (Int, [a])
> findSoln k f ecs = 
>     let n = length ecs
>         bs = map (`elem` freeInds n k) [0..length ecs-1]
>         lens = map length ecs 
>         g i = zip3 bs i lens
>         frec j iCurr = 
>             let iNext = findNext2 k $ g iCurr 
>                 soln = zipWith (!!) ecs iCurr
>             in  if last iCurr < 0 then error "No more solutions." else
>                 if f soln then (j, soln) else frec (j+1) iNext
>     in  frec 0 $ take (length ecs) $ repeat 0 

> findSoln2 :: (NFData a, Eq a, Show a) => 
>     Constraints -> Predicate [a] -> [[a]] -> IO (Int, [a])
> findSoln2 k f ecs = 
>     let initVal = take (length ecs) $ repeat 0
>         bs = map (`elem` freeInds (length ecs) k) [0..length ecs-1]
>         lens = map length ecs 
>         g i = zip3 bs i lens
>         frec j iCurr = do
>             let iNext = findNext2 k (g iCurr)
>                 soln = makeSoln 0 ecs iCurr
>                 jNext = j+1
>             if last iCurr < 0 then error "No more solutions." else
>                 if f (force soln) then return (j, soln) else -- SEE NOTE BELOW
>                     if j `mod` printIter == 0 && printIt then 
>                             putStrLn ("Solutions examined: "++show j) >> 
>                             frec jNext iNext
>                     else frec jNext iNext
>     in  frec 0 initVal 

Note on use of force in findSoln2: if force is not used and used exactly where
it has been placed above, the function experiences a thunk leak. Printing iCurr, 
iNext, or soln can help to knock back memory usage, but it still leaks until 
printed unless force is used. Using seq can slow the leak to a trickle, but it 
still occurs and will cause long searches to crash. Using force appears to be
the best solution for this leak.


> findSolnPar :: (NFData a, Eq a, Show a) => 
>     Constraints -> Predicate [a] -> [[a]] -> Int -> IO (Int, [a])
> findSolnPar k f ecs parSize = 
>     let initVal = take (length ecs) $ repeat 0
>         bs = map (`elem` freeInds (length ecs) k) [0..length ecs-1]
>         lens = map length ecs 
>         g i = zip3 bs i lens
>         frec j iCurr = do
>             let iCurrs = filter ((>=0).last) (iCurr : findNextI k (g iCurr) (parSize - 1))
>                 iNext = findNext2 k $ g $ last iCurrs -- for next iteration
>                 solns = filter f $ seq (force iCurrs) 
>                         (map (makeSoln 0 ecs) iCurrs `using` parList rdeepseq)
>                 jNext = j+length iCurrs
>             if last iCurr < 0 then error "No more solutions." else
>                 if not $ null solns then return (j, head solns) else
>                     if printIt then 
>                         putStrLn ("Solutions examined: "++show j) >> frec jNext iNext
>                     else frec jNext iNext
>     in  frec 0 initVal


> makeSoln :: Int -> [[a]] -> [Int] -> [a]
> makeSoln j [] [] = []
> makeSoln j [] is = error "(makeSoln) Not enough equivalence relations!"
> makeSoln j ecs [] = error "(makeSoln) Not enough indices!"
> makeSoln j (e:ecs) (i:is) = 
>     if i < length e then (e !! i) : makeSoln (j+1) ecs is
>     else error ("Bad index at position "++show j++": "++show i++", classes="++show (length e))

> fetch xs i = if i >= length xs then error "(fetch) Index is too large!" else xs !! i


The findNext function performs the actual traversal of the space. It bypasses
a lot of irrelevant points by only looking at points that at least satisfy 
the let-in constraints. Those points may or may not satisfy the Predicate, f, 
and so the function may have to explore many solutions.


> findNext :: Constraints -> [Index] -> [Bound] -> [Index]
> findNext k is lens = 
>     let bs = map (`elem` freeInds (length is) k) [0..length is-1]
>         xs = zip3 bs (is) (lens)
>     in  foldl applyCons (incr xs) k 


This version is used for one of the recursive implementations.

> findNext2 :: Constraints -> [(Bool, Index, Bound)] -> [Int]
> findNext2 k xs = foldl' applyCons (incr2 xs) k

> findNextI :: Constraints -> [(Bool, Index, Bound)] -> Int -> [[Index]]
> findNextI k xs i = 
>     let xs' = findNext2 k xs
>         fixWith = zipWith (\(a,b,c) d -> (a,d,c))
>     in  xs' : take (i-1) (iterate (findNext2 k . fixWith xs) xs') where 

The applyCons function applys let-in constraints to an index list. Indices
on the left are given preference when satisfying constraints.

> applyCons :: [Int] -> [(Int, Int)] -> [Int]
> applyCons inds [] = inds
> applyCons inds ((i,j):ijs) = 
>     foldl (f val) inds (map fst ijs) where -- might need seq here
>     val = (take (j-i+1) $ drop i inds)
>     f val src i = take i src ++ val ++ 
>                   drop (i + length val) src
	
Solutions are explored by keeping track of a list of indices into equivalence
classes. This list is incremented by considering which indices can actually 
be changed (those unconstrained by let-in expressions). If the end of the 
search space is reached, an error is thrown since there is no solution.	



The following modification avoids a memory error if used for traversal
with something like iterate. It returns a "flag" at the end (-1) if 
no solutions exist. This must be picked up by the calling functions for
termination to occur.

> incr :: [(Bool, Int, Int)] -> [Int]
> incr ((b,i,l):xs) = let is = map (\(x,y,z) -> y) xs in
>     if b then if i >= l-1 then 0 : incr xs else (i+1) : is
>     else i : incr xs
> incr [] = error "No more solutions!"


The following version uses a flag instead of an error message:

> incr2 :: [(Bool, Int, Int)] -> [Int]
> incr2 ((b,i,l):xs) = let is = map (\(x,y,z) -> y) xs in
>     if b then if i >= l-1 then 0 : incr2 xs else (i+1) : is
>     else i : incr2 xs
> incr2 [] = [-1] -- no more solutions exist


The freeInds function turns let-in constraints into a list of indices that
can be updated or incremented.

> freeInds :: Int -> Constraints -> [Int]
> freeInds n k = 
>     let k' = map (map (\(i,j) -> [i..j])) k 
>         t = nub $ concat $ concatMap tail k'
>     in  filter (not . (`elem` t)) [0..n-1] 


==================================

THIS VERSION OF GREEDY PROG DOES NOT WORK - LETS ARE NOT DONE RIGHT

A version of greedyProg to support Let statements. Constraints are satisfied from 
left to right. Breaks are likely to occurr with repeats. For example, with the
progression

	let x = ... in x x

a break in voice-leading behavior is likely to occur between the last chord of the
first instance of x and the first chord of the second instance of x. There is 
currently no way around this using the greedy approach. The more exact searches
further up in this file are alternatives in such cases.

> greedyLet :: (Eq a, Show a) => Predicate (a,a) -> Fallback a -> Constraints -> 
>              [EqClass a] -> StdGen -> [a]
> greedyLet p f k es g = 
>     let n = length es
>         cs = greedyProg' p f g es
>         consPat = foldl applyCons [0..n-1] (sort k) 
>     in  map (cs !!) consPat

> greedyLetT :: QSpace AbsChord -> EqRel AbsChord -> Predicate (AbsChord,AbsChord) -> 
>               Fallback AbsChord -> Constraints -> [TChord] -> StdGen -> [TChord]
> greedyLetT q r p f k cs g = 
>     let justCs = map tnP cs
>         es = map (eqClass q r) justCs
>         justCs' = greedyLet p f k es g
>     in  zipWith newP cs justCs'
