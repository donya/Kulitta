PTGG Generation module for creating training data sets
Donya Quick
Last modified: 19-Dec-2014

> module TemporalGen where
> import PTGG
> import MusicGrammars 
> import Data.List
> import System.Random
> import InsideOutside


> type TSym a = (a, Rational)
> type RHS a = [TSym a]
> type TRule a = (a, RHS a)


> tSeed = [i (defMP{dur=maxDur})] :: Sentence CType MP
> maxDur = 1.0
> minDur = sn
> dataPoints = 1000
> rSeed = 0
> iters = 4
> useLets = False 

> infInts g = 
>     let (g1, g2) = split g
>         (i, g') = next g
>     in  i : infInts g2

Generating a data set from the 

gen (rRules1 minDur useLets) iters i Major tSeed

> genData = 
>     let f i = snd (gen (rRules1 minDur useLets) (mkStdGen i, tSeed) !! iters) 
>         theData = map f $ take dataPoints $ infInts (mkStdGen rSeed)
>     in  map toPairs theData

> writeData = writeFile "tdata.txt" $ concat $ intersperse "\n" $ map show genData


======================

> convRule :: (Prob, TRule a) -> Rule a MP
> convRule (p, (lhs, rhs)) = (lhs, p) :-> toRuleFun rhs

> toRuleFun :: [(a, Dur)] -> RuleFun a MP
> toRuleFun rhs p = map (\(c,d) -> (NT (c, dFac d p))) rhs

> readTemporalRules :: FilePath -> IO ((CType, Dur), [(Prob, TRule CType)])
> readTemporalRules fp = do
>     str <- readFile fp
>     let theLines = lines str
>         startSym = read (head theLines) :: (CType, Dur)
>         theRules = map (read) $ tail $ theLines
>     return (startSym, theRules)

> readTemporalRules2 :: FilePath -> IO ((CType, Dur), [Rule CType MP])
> readTemporalRules2 fp = do
>     (startSym, theRules) <- readTemporalRules fp
>     return (startSym, map convRule theRules)

> readPTGG = readTemporalRules2


> readTemporalData :: FilePath -> IO [[(CType, Dur)]]
> readTemporalData fp = do
>     str <- readFile fp
>     return $ map (read) $ lines str

> showTemporal :: ((CType, Dur), [TRule CType]) -> String
> showTemporal (startSym, rules) = 
>     show startSym ++ "\n\n" ++
>     concat(intersperse "\n" $ map showTRule rules)

> showTRule (lhs, rhs) = show lhs ++ "  ->  "++ 
>     concat (intersperse "  " $ map show rhs)

> printTemporal = putStrLn . showTemporal

gen (map (toRelDur2 minD) rs) numIters i Major tSeed
 
> genFromFile fpIn fpOut minD dataAmt seed numIters = do
>     (startSym, rs) <- readTemporalRules2 fpIn
>     let  f i = snd (gen (map (toRelDur2 (<minD)) rs) (mkStdGen i, tSeed) !! numIters)
>          theData = map f $ take dataPoints $ infInts (mkStdGen seed)
>          theData' = map toPairs theData
>     writeFile fpOut $ concat $ intersperse "\n" $ map show theData'

> genDataTest = genFromFile "trules.txt" "tdata.txt" 0.01 1000 0 3

> genDataTest2 = genFromFile "trules2.txt" "tdata2.txt" 0.01 1000 0 3

> trules2 = genFromFile "trules2.txt" "tdata2.txt" minDur dataPoints 4 iters

> trules3 = genFromFile "learning\\trules3.txt" "learning\\tdata3.txt" minDur dataPoints 4 iters
