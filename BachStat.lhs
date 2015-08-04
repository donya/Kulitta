Statistics Gathering Module for Bach Chorales Data
Donya Quick
Last Modified: 18-Dec-2014

> module BachStat where
> import PTGG 
> import MusicGrammars
> import System.Random
> import Learning
> import PCFGtoPTGG
> import MusicGrammars
> import ChordSpaces hiding (i)
> import Data.List
> import PostProc
> import Constraints
> import Search

> rs = [I,II,III,IV,V,VI,VII]

> phraseCounts m = do
>     s <- readFile "BachChords.txt"
>     let dstr = map (\(a,b,c) -> c) $ filter (\(a,b,c) -> b==m) $ parseData s
>         counts = length dstr
>     putStrLn (show counts)
>     putStrLn "Done."

> singleStats m = do
>     s <- readFile "BachChords.txt"
>     let dstr = concatMap (\(a,b,c) -> c) $ filter (\(a,b,c) -> b==m) $ parseData s
>         counts = map (\x -> length $ filter (==x) dstr) rs
>     writeFile ("BachStatsSingle_"++show m++".txt") (concat $ intersperse "\n" $ map show counts)
>     putStrLn "Done."

> readSingle :: IO [Int]
> readSingle = readFile "BachStatsSingle.txt" >>= (return . map read . lines)

> pairStats m = do 
>     s <- readFile "BachChords.txt"
>     let dstrs = map (\(a,b,c) -> c) $ filter (\(a,b,c) -> b==m) $ parseData s
>         pats = [(a,b) | a<-rs, b<-rs]
>         counts = map (\p -> sum $ map (countPairs p) dstrs) pats
>     writeFile ("BachStatsPairs_"++show m++".txt") (concat $ intersperse "\n" $ map show $ zip pats counts)
>     putStrLn "Done."


> pairStatsRare c m = do 
>     s <- readFile "BachChords.txt"
>     let dstrs = map (\(a,b,c) -> c) $ filter (\(a,b,c) -> b==m) $ parseData s
>         pats = [(a,b) | a<-rs, b<-rs]
>         counts = map (\p -> sum $ map (countPairs p) dstrs) pats
>         cps = zip pats counts
>         cps' = filter ((<c).snd) cps
>     writeFile ("BachStatsPairs_"++show m++"_less"++show c++".txt") (concat $ intersperse "\n" $ map show cps')
>     putStrLn "Done."

> countPairs :: (Eq a) => (a,a) -> [a] -> Int
> countPairs (a,b) (x:y:t) = (if a==x && b==y then 1 else 0) + countPairs (a,b) (y:t)
> countPairs _ _ = 0


========================

The following filters out chord transitions that were relatively rare
in the data set, based on the suspicion that they might have been due
to noise (mis-labeled chords) or improper identification of phrase 
boundaries in the corpus.

> okRTrans :: Mode -> Predicate (CType, CType)
> okRTrans m x = not $ elem x $ vals m where
>     vals Minor = [(II,VI),   (II,VII),  (III,II), (III,V), 
>                   (III,VI),  (III,VII), (IV,III), (IV,VI),
>                   (V,III),   (V,VII),   (VI,III), (VI,IV), 
>                   (VI,V),    (VI,VII),  (VII,II), (VII,IV),
>                   (VII,VI)]
>     vals Major = [(III,II),  (III,VII), (IV,III), (IV,VI),
>                   (V,VII),   (VI,III),  (VI,VII), (VII,III),
>                   (VII,IV),  (VII,V),   (VII,VI), (VII,VII)]


> tsdSpace :: Mode -> QSpace CType
> tsdSpace m = [f I 0 ++ f III 2 ++ f VI 5,
>                f IV 3 ++ f II 1,
>                f V 4 ++ f VII 6] where
>     rCounts = if m==Major then [7833, 3018, 947, 2576, 5723, 1619, 791]
>               else [4925, 1425, 504, 1113, 2726, 442,  418]
>     f x i = take (rCounts !! i) $ repeat x

       I     II    III  IV    V     VI    VII
Major: 7833, 3018, 947, 2576, 5723, 1619, 791
Minor: 4925, 1425, 504, 1113, 2726, 442,  418


> expandTSD2 :: QSpace CType -> Predicate (CType, CType) -> StdGen -> [(Key, Dur, CType)] -> 
>              [(Key, Dur, CType)]
> expandTSD2 tsdSpace' p g xs = 
>     let xs' = map thd xs
>         n = length xs
>         es = (map (eqClass tsdSpace' tsdEq) $ take (n-1) xs') ++ [[last xs']]
>     in  zipWith (\(a,b,c) d -> (a,b,d)) xs $ greedyProg' p tsdFall g es 

> tsdEq :: EqRel CType
> tsdEq a b = or $ map (\e -> elem a e && elem b e) (tsdSpace Major)

> tsdFall :: Fallback CType
> tsdFall es g x = (g,x)