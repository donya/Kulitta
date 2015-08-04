Mode Space Implementation
Donya Quick
Last modified: 19-August-2014

Based on mode space implementation for doctoral thesis.

Major changes since last version:
- Changed occurrences of AbsPitch to PitchNum to avoid conflict with Euterpea's AbsPitch

> module ModeSpace where
> import ChordSpaces
> import Data.List
> import System.Random
> import Constraints

> type AbsMode = [PitchNum]
> type JChord = (AbsChord, AbsMode)

> modeEq :: EqRel JChord
> modeEq a b = normO(snd a) == normO(snd b)

The set of all modes rooted at 0

> allModes :: [AbsMode]
> allModes = allRots [0,2,4,5,7,9,11] where
>     allRots x = take 7 $ iterate doRot x
>     doRot x = normO $ normT (tail x ++ [head x + 12])

> allKModes = map (normO . uncurry t) [(k,m) | k<-[0..11], m<-allModes]

> allJChords :: [JChord]
> allJChords = 
>     let masks = makeRange (take 7 $ repeat (0,1)) 
>         applyMask (f, m) = (map snd $ filter ((>0).fst) $ zip f m, m)
>     in  map applyMask [(f,m) | f<-masks, m<-allKModes]

> modeSpace :: QSpace JChord
> modeSpace = allJChords // modeEq

This version allows more efficient specification of modal space 
involving only chords fitting certain templates. 

> modeSpace' :: [[Int]] -> QSpace JChord
> modeSpace' temps = filter (f temps) allJChords // modeEq where
>     f temps (c,m) = elem c $ map (toTemp m) temps
>     toTemp m t = map (m!!) t

For example, to get only triads with the root, third, and fifth,
one would use: modeSpace' [[0,2,4]].

Redefinition of nearest neighbor for modal chords:

> nearFallJ ::  EqClass JChord -> StdGen -> JChord -> (StdGen, JChord)
> nearFallJ e g (x,m) = 
>     let ds = map (simpleDist x) (map fst e) :: [Double]
>         y = e !! (head $ findIndices (==minimum ds) ds)
>     in  (g, y)


