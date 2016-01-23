> module Kulitta.QuotientSpaces where
> import Data.List
> import System.Random
> import Control.DeepSeq
> import Data.Maybe


> type EqClass a = [a] -- equivalence class
> type QSpace a = [EqClass a] -- quotient space
> type Predicate a = a -> Bool
> type Norm a = a -> a -- normalizations
> type EqRel a = a -> a -> Bool -- equivalence relations

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