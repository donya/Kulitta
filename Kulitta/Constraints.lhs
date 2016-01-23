Module for musical constraints
Donya Quick
Last modified: 18-Aug-2014

> module Kulitta.Constraints where
> import Kulitta.ChordSpaces
> import Data.List
> import System.Random
> import Data.Maybe

Musical constraints are modeled as predicates over some number of
chords. These are "hard" constraints such that a piece of music 
either does or does not satisfy the constraints. 

======== PREDICATES =========

First we defint the hProg function that turns a pairwise predicate 
into a progression predicate.

> hProg :: Predicate (a,a) -> Predicate [a]
> hProg f xs = and $ map f $ zip xs $ tail xs

> foldPreds :: [Predicate a] -> Predicate a
> foldPreds fs xs = and $ map ($xs) fs

Here we define the notion of voices not crossing to mean that if there 
is a permutation that sorts both chords, the voices do not cross. This 
allows for voices that may be in unison.

 hNotCross :: Predicate (AbsChord, AbsChord)
 hNotCross (c1,c2) = 
     let ps1 = findAllP c1 (sort c1) -- find permutation, p1, to sort c1
         ps2 = findAllP c2 (sort c2) -- find permutation, p2, to sort c2
     in  not $ null [p | p<-ps1, elem p ps2] 

> hNotCross :: Predicate (AbsChord, AbsChord)
> hNotCross (c1,c2) = 
>     let sn = permutations [0..length c1-1]
>         ps1 = filter (\s -> p s c1 == sort c1) sn -- find permutation, p1, to sort c1
>         ps2 = filter (\s -> p s c2 == sort c2) sn -- find permutation, p2, to sort c2
>     in  not $ null [p | p<-ps1, elem p ps2]  

Alternatively, we may wish to not allow voices to touch either. If
we know the chords are both sets (no duplicated pitches), then ranks 
can be compared.

> hNotCross' :: Predicate (AbsChord, AbsChord)
> hNotCross' (c1,c2) = rank c1 == rank c2


The function above needs to generate all permutations (as indices) 
of a chord.

> findAllP :: AbsChord -> AbsChord -> [[Int]]
> findAllP c1 c2 = 
>     let n = length c1
>         f i = filter (\j -> c2 !! j == c1 !! i) [0..n - 1]
>         g [] = [[]]
>         g (is:t) = [(a:b) | a<-is, b<-g t]
>     in  filter (\x -> x == nub x) (g $ map f [0..n - 1])

> hNotCrossP :: Predicate Prog
> hNotCrossP = hProg hNotCross

Now we define a predicate for avoiding parallel motion (all voices 
moving in the same direction). We will consider parallel motion in 
the context of vectors, so this is not the strictest possible case.
We don't care about intervals of 0, since that does not constitute
voice movement.

> hNotPar1 :: Predicate (AbsChord, AbsChord)
> hNotPar1 (c1, c2) = 
>     let diffs = zipWith subtract c1 c2
>     in  not $ hasDups $ filter (/= 0) diffs

> hasDups :: (Eq a) => [a] -> Bool
> hasDups [] = False
> hasDups (a:as) = elem a as || hasDups as

Now we define a stricter version that considers parallel motion
in MULTISETS rather than vectors. On more than 2 voices, this 
will become rather hard to satisfy unless the chords are 
quite strange.

> hNotParStrict1 :: Predicate (AbsChord, AbsChord)
> hNotParStrict1 (c1,c2) = hNotPar1 (sort c1, sort c2)

And finally, progression level predicates of each.

> hNotPar2, hNotParStrict2 :: Predicate Prog
> hNotPar2 = hProg hNotPar1
> hNotParStrict2 = hProg hNotParStrict1


And now a "fill in the blanks" predicate. With this predicate,
we can set start and end points, middle points, etc. if we 
want them. Of course, there will be no valid solutions unless we
pick points within the intended quotient space.

> fillBlanks :: [Maybe AbsChord] -> Predicate Prog
> fillBlanks (m:ms) (p:ps) = 
>     case m of Just c -> p == c && fillBlanks ms ps
>               Nothing -> fillBlanks ms ps 
> fillBlanks [] [] = True
> fillBlanks _ _ = False


Now some distance metric-type approaches.

> type DistMeasure = AbsChord -> AbsChord -> Double
> type Threshold = Double

> simpleDist, eucDist, maxDist :: DistMeasure
> simpleDist a b = fromIntegral $ sum $ map abs $ zipWith subtract a b
> eucDist a b = sqrt $ sum $ map fromIntegral $ zipWith subtract a b
> maxDist a b = fromIntegral $ maximum $ map abs $ zipWith subtract a b

> distClass :: DistMeasure -> Predicate Double -> Predicate (AbsChord, AbsChord)
> distClass d ft (x,y) = ft $ d x y

> simpleClass t = distClass simpleDist (<=t)
> eucClass t = distClass eucDist (<=t)
> maxClass t = distClass maxDist (<=t)

> noCPL :: Double -> Predicate (AbsChord, AbsChord)
> noCPL i x = maxClass i x && hNotPar1 x && hNotCross x

> noCL :: Double -> Predicate (AbsChord, AbsChord)
> noCL i x = maxClass i x && hNotCross x

> progL t =  hProg (maxClass t) 


===========================

Contour equivalence

> rank :: [PitchNum] -> [Int]
> rank xs = 
>     let vals = sort $ nub xs
>         ranks = zip vals [0..length vals - 1]
>     in  map (\x -> fromJust $ lookup x ranks) xs



============================

ADDITIONAL THESIS PREDICATES

--- Single chords ---

> sorted :: Predicate AbsChord
> sorted x = x == sort x

> spaced :: [(Int, Int)] -> Predicate AbsChord
> spaced lims x = and $ 
>     zipWith (\(l,u) diff -> l <= diff && diff <= u) lims $ 
>     zipWith subtract x (tail x)

> triads :: [AbsChord]
> triads = [[0,0,4,7], [0,4,7,7], [0,0,3,7], [0,3,7,7], [0,0,3,6]]

> doubled :: [AbsChord] -> Predicate AbsChord
> doubled templates x = elem (normOP x) allTriads where
>     allTriads = concatMap (\c -> map (normOP . t c) templates) [0..11]

> satbFilter x = and $ map ($x) [sorted, spaced satbLimits, doubled triads]
> satbFilter2 x = and $ map ($x) [sorted, spaced satbLimits]
> satbLimits = repeat (3,12)
> satbRanges = [(40,60), (47,67), (52,76), (60,81)]
> satbChords = filter satbFilter (makeRange satbRanges)

> satbOP :: QSpace AbsChord
> satbOP = satbChords // opEq where

> satbOP' :: StdGen -> QSpace AbsChord
> satbOP' g = randomize g satbChords // opEq where

> satbR :: StdGen -> Predicate AbsChord -> EqRel AbsChord -> QSpace AbsChord
> satbR g f r = randomize g (filter f $ makeRange satbRanges) // r 

> pianoChord :: Predicate AbsChord
> pianoChord x = length x <= 5 &&  maximum x - minimum x <= 12

--- Pairs ---

> notParallel :: Predicate (AbsChord, AbsChord)
> notParallel (x,y) = let diff = zipWith subtract x y in nub diff == diff

> voiceLeading :: [Predicate (PitchNum, PitchNum)] -> Predicate (AbsChord, AbsChord)
> voiceLeading preds (x,y) = and $ zipWith ($) preds $ zip x y


> vl7 :: Predicate (AbsChord, AbsChord)
> vl7 = voiceLeading (repeat f) where f (a,b) = abs(a - b) <= 7

Note: the version of noCrossing below assumes that voices cannot overlap.
In other words, the pitches must all be unique.

> noCrossing :: Predicate (AbsChord, AbsChord)
> noCrossing (x,y) = rank x == rank y

> pairProg2 :: (Eq a, Show a) => QSpace a -> EqRel a -> Predicate (a,a) -> [a] -> [[a]]
> pairProg2 qs r c [] = [[]]
> pairProg2 qs r c (x:xs) = 
>     let newSolns = [(y:ys) | y<-eqClass qs r x, ys<-pairProg2 qs r c xs, c (y, head ys)]
>     in  if not $ null newSolns then newSolns
>         else error "No solutions that satisfy the consraints!"


===============