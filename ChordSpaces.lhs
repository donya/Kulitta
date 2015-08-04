Chord Spaces Implementation
Donya Quick and Paul Hudak
Last modified: 19-Dec-2014

This module is based on several earlier versions that were for:
1. "Computing with Chord Spaces" (ICMC 2012) 
2. "A Temporal Generative Graph Grammar for Harmonic and 
   Metrical Structure" (ICMC 2013)
3. The dissertation titled "Kulitta: a Framework for Automated
   Music Composition" (Yale 2014)
   
Major changes since the last version:
- AbsPitch renamed to PitchNum to avoid conflicts with Euterpea's AbsPitch

> module ChordSpaces where
> import Data.List
> import System.Random
> import Control.DeepSeq
> import Data.Maybe

Type definitions:

> type PitchNum = Int -- same as Euterpea's AbsPitch
> type Norm a = a -> a -- normalizations
> type EqRel a = a -> a -> Bool -- equivalence relations
> type AbsChord = [Int]
> type Prog = [AbsChord] -- Chord progression
> type EqClass a = [a] -- equivalence class
> type QSpace a = [EqClass a] -- quotient space
> type Predicate a = a -> Bool

The makeRange function will generate Z^n for user-specified ranges.

> makeRange :: [(PitchNum, PitchNum)] -> [AbsChord]
> makeRange = foldr (\(l,u) xs -> [(a:b) | a<-[l..u], b<-xs]) [[]]


A version of makeRange for use with sorted spaces:

> makeRange' :: [(PitchNum, PitchNum)] -> [AbsChord]
> makeRange' = foldr (\(l,u) xs -> [(a:b) | a<-[l..u], b<-xs, psort (a:b)]) [[]] where
>     psort (a:b:t) = a<b
>     psort _ = True 


========= O, P, & T IMPLEMENTATION =========

First we will define the octave and transposition operations. 
For f(x)=y with f in {o, t, p}, x~y for the corresponding 
equivalence relation (O, T, and P respectively).

> o,p :: [Int] -> AbsChord -> AbsChord
> o = zipWith (\i x -> x + 12 * i)
> p s xs = map (xs !!) s

> t :: Int -> AbsChord -> AbsChord
> t c = map (+c)

Note: "inv" below is just called "i" in the dissertation. It 
is called "inv" here for clarity.

> inv :: Bool -> AbsChord -> AbsChord
> inv neg = if neg then map (*(-1)) else id

We define normalizations for O, P, T, OP, OT, and PT.
We also add a new definition, OPC.

> normO, normT, normP, normOP, normPT, normPC, normOPC :: Norm AbsChord
> normO = map (`mod` 12)
> normT x = map (subtract $ head x) x
> normP = sort
> normOP = sort . normO
> normPT = normT . sort
> normOT = normO . normT
> normPC = nub . normP
> normOPC = nub . normOP
> normOC = normC . normO

> normC :: AbsChord -> AbsChord
> normC (x1:x2:xs) = 
>     if x1 == x2 then normC (x2:xs) else x1 : normC (x2:xs)
> normC x = x

Given a normalization, it can be turned into an 
equivalence relation.

> normToEqRel :: (Eq a) => Norm a -> EqRel a
> normToEqRel f a b = f a == f b

> oEq, pEq, tEq, opEq, ptEq, opcEq :: EqRel AbsChord
> [oEq, pEq, tEq, opEq, ptEq, otEq, opcEq] = 
>     map normToEqRel [normO, normT, normP, normOP, normPT, normOT, normOPC]

Old version of optEq that checks all octave stacks:

> optEq' :: EqRel AbsChord
> optEq' a b = 
>     let (a', b') = (normT $ normOP a, normT $ normOP b)
>         s = map (normT . normP) $ octStacks b'
>     in  or (map (==a') s)

New version that only checks rotations:

> optEq :: EqRel AbsChord
> optEq a b = 
>     let n = length b
>         (a', b') = (normT $ normOP a, normT $ normOP b)
>         is = map (\k -> take k (repeat 1) ++ take (n - k) (repeat 0)) [0..n]
>         s = map (normT . normP) $ map (\i -> o i b') is
>     in  or (map (==a') s)

> octStacks :: AbsChord -> [AbsChord]
> octStacks x = zipWith o (makeRange $ take (length x) $ repeat (0,1)) (repeat x)

> normOPT :: Norm AbsChord
> normOPT x = 
>     let x' = normT $ normOP x
>         s = map (normT . normP) $ octStacks x'
>     in  head $ sort s

The above can also use "sortBy optComp" instead of "sort" to achieve a
slightly different normalization approach that is more similar to 
the fundamental domain for OPT given by Callender et al.

> optComp a b = 
>     let (a',b') = (toIntervals a, toIntervals b)
>     in  if a' == b' then compare a b else compare a' b' 

> toIntervals x = zipWith subtract x (tail x)
  
OPTC-equivalence can be implemented similarly to OPT-equivalence.  
  
> optcEq :: EqRel AbsChord
> optcEq a b = optEq (normOPC a) (normOPC b)

> normOPTC :: AbsChord -> AbsChord
> normOPTC = normOPT . normOPC



========= QUOTIENT SPACE IMPLEMENTATION =========

First we define the "slash" operator for S/R. 

> (//) :: (Eq a) => [a] -> EqRel a -> QSpace a
> [] // r = []
> xs // r = 
>     let sx = [y | y <- xs, r y (head xs)] 
>     in  sx : [z | z <- xs, not (elem z sx)] // r

The eqClass function is used to find the equivalence class of an
element, x, given a quotient space and the relation used to form it.
We need to know the relation used, because we do not require that
x is in the quotient space, qs.

> eqClass :: (Eq a, Show a) => QSpace a -> EqRel a -> a -> EqClass a
> eqClass qs r x = 
>     let ind = findIndex (\e -> r x (head e)) qs
>     in  maybe (error ("(eqClass) No class for "++show x)) (qs !!) ind 

=============================

Code for randomizing a list.

> randomize :: StdGen -> [a] -> [a]
> randomize sg rs = 
>     let n = length rs
>         plist = take n (nub (randomRs (0,n-1) sg))
>     in  map (rs!!) plist

