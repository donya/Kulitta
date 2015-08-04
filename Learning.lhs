Main module for learning PCFGs and PTGGs from data sets
Donya Quick
Last mofidied: 19-Dec-2014

Implementation for learning chapter in doctoral thesis.

> module Learning where
> import InsideOutside 
> import MusicGrammars
> import CykParser
> import System.Random
> import Data.List
> import ChordSpaces
> import Parser
> import System.Environment
> import System.IO.Unsafe
> import PTGG
> import TemporalGen


Modifications:
- Added support for temporal rules forking decision process

Datatype definitions

> version = "9g17c"

CONSTANTS

> logData = True -- whether to log command prompt output

DISAMBIGUATIONS

> type RRule a = Parser.Rule a
> type PRule a = (Double, (a, [a]))

DATA TYPES

> data GramMode = PCFG | Temporal
>   deriving (Eq, Show, Ord, Read)

> data Grammar = GramPCFG (RTerm, [RRule RTerm]) | GramOther

> data DataSet = DataPCFG [(Int, Mode, [CType])] | DataOther

> data DistMode = Uniform | Random Int
>   deriving (Eq, Show, Ord, Read)

> data Params = Params {
>     confFile :: String, -- name of configuration file
>     dataFile :: String, -- name of data file
>     gramFile :: String, -- name of grammar definition file
>     gramMode :: GramMode, -- grammar mode, PCFG or Temporal
>     runIters :: Int, -- number of learning iterations
>     convThresh :: Double, -- convergence threshold
>     dataSeeds :: [Int], -- data sets to use during testing (one or more)
>     initDist :: DistMode, -- initial probability distribution for rule set
>     dataMode :: [Mode], -- what modes are in the data? (some combination of Major and Minor)
>     maxLength :: Int, -- maximum length strings to select from data set
>     sampleSize :: Int, -- sample size to choose from the data set
>     filterData :: Bool, -- True if filtering data points based on length
>     outNamePre :: String, -- optional additional prefix for output files
>     minProb :: Double, -- minimum probability to allow for rules
>     logFile :: String -- name of log file (to be set automatically)
>     }



========================================
Code for writing output file of results

STUFF TO REDEFINE EVENTUALLY:

startSym = TR
rules = iotr
	
	
> mainL = do
>     a <- getArgs 
>     let fname = if null a then error "No config file!" else a !! 0
>     pf <- readFile fname
>     let p = parseCFile fname pf
>     case (gramMode p) of PCFG -> mainPCFG p
>                          Temporal -> mainTemporal p

> fixLHS (a,bs) = ((a,1.0::Rational), bs)

> putLogLn logfile str = do
>     putStrLn str
>     if logData then appendFile logfile (str++"\n") else return ()

> putLog logfile str = do
>     putStr str
>     if logData then appendFile logfile str  else return ()

> errorLog logfile str = do
>     if logData then appendFile logfile str else return ()
>     error str

> mainTemporal p = do
>     (startSym, rules0) <- readTemporalRules $ gramFile p
>     chords <- readTemporalData $ dataFile p
>     let rules = map (snd) rules0
>         rules' = map fixLHS rules :: [RRule (CType, Rational)]
>     printTemporal (startSym, rules)
>     let ds = dataSeeds p
>     putLogLn (logFile p) "Initializing production probs..."
>     let initQs = makeQsT p rules'
>         env = TOracle initQs startSym (ruleGroups2 rules') (findByRHS2 rules')
>         chords' = if filterData p then filter ((<=maxLength p).length) chords else chords
>         fRun = mainSub2 p rules' startSym chords' initQs env
>     putLogLn (logFile p) "Running..."
>     putLogLn (logFile p) ("Total data points: "++show (length chords))
>     putLogLn (logFile p) ("Average progression length: "++show(averageLength chords))
>     allQs <- seq chords (sequence $ map fRun $ [0..length ds-1])
>     putLogLn (logFile p) "\n\nSummarizing results..."
>     writeFile (makePre (confFile p) ++ outNamePre p ++ "final.txt") $ mkQTableT ds rules' allQs
>     putLogLn (logFile p) "Done.\n"

> durAlph = recSum minDur maxDur where
>     recSum x m = if x >= m then [m] else x : recSum (2*x) m

> -- for temporal grammars
> mainSub2 p theRules startSymb allData initQs theEnv num = do 
>     let theSeed = (dataSeeds p !! num)
>     putLogLn (logFile p) "===================" 
>     putLogLn (logFile p) ("Seed: " ++ show theSeed)
>     putLogLn (logFile p) "Starting..."
>     let chordsTaken = randomData (sampleSize p) theSeed allData
>     writeFile (sampleFile p theSeed) $ show chordsTaken
>     putLogLn (logFile p) ("Randomly taken data points: "++show (length chordsTaken))
>     putLogLn (logFile p) ("Average progression length: "++show(averageLength chordsTaken))
>     putLogLn (logFile p) ("Maximum progression length: "++show(maximum $ map length chordsTaken))
>     putLogLn (logFile p) ("Minimum progression length: "++show(minimum $ map length chordsTaken))
>     qs <- learnProbs theEnv allData (runIters p) (minProb p) (convThresh p)
>     let qsAll = phis theEnv : qs
>         prsAll = map (\q -> zip q theRules) qsAll
>         prs = last prsAll
>         resultFile = makePre (confFile p) ++ outNamePre p ++ "s"++show theSeed ++".txt"
>         ruleStrs = mkQStrT prs
>     putLogLn (logFile p) "Writing results..."
>     writeFile resultFile (stats theSeed p 
>                           ++ "Results: \n"++ruleStrs ++ 
>                           "\n=============\n\n" ++ mkQTableT [0..(runIters p)] theRules qsAll) -- allQs prsAll)
>     putLogLn (logFile p) "Done."
>     return $ last qs

> sampleFile p s = reverse (drop 4 $ reverse $ confFile p) ++ "_data" ++ 
>                  show s ++ ".txt"


> mainPCFG p = do
>     (startSym, rules) <- readPCFG $ gramFile p
>     chords <- readData2 $ dataFile p
>     printPCFG (startSym, rules)
>     let ds = dataSeeds p
>     putLogLn (logFile p) "Initializing production probs..."
>     let initQs = makeQs p rules
>         bachChords = procChords2 chords p rules startSym 
>         env = TOracle initQs startSym (ruleGroups1 rules) (findByRHS1 rules)
>         fRun = mainSub1 p rules startSym bachChords initQs env
>     putLogLn (logFile p) ("Total data points: "++show (length bachChords))
>     putLogLn (logFile p) ("Average progression length: "++show(averageLength bachChords))
>     putLogLn (logFile p) ("Maximum progression length: "++show(maximum $ map length bachChords))
>     putLogLn (logFile p) ("Minimum progression length: "++show(minimum $ map length bachChords))
>     putLogLn (logFile p) "Running..."
>     allQs <- seq bachChords (sequence $ map fRun $ [0..length ds-1])
>     putLogLn (logFile p) "\n\nSummarizing results..."
>     writeFile (makePre (confFile p) ++ outNamePre p ++ "final.txt") $ mkQTable ds rules allQs
>     putLogLn (logFile p) "Done.\n"

> averageLength lists = 
>     let lens = map (fromIntegral.length) lists
>     in  sum lens / fromIntegral (length lists)

> getGrammar p = case (gramMode p) of
>     PCFG -> do
>             g <- readPCFG $ gramFile p 
>             return $ GramPCFG g
>     _ -> errorLog (logFile p) "Only PCFGs are handled for now."

> getData p = case (gramMode p) of
>     PCFG -> do
>             g <- readData $ dataFile p 
>             return $ DataPCFG g
>     _ -> errorLog (logFile p) "Only PCFGs are handled for now."


> makeQs p rules = case initDist p of
>     Uniform -> uniQs (ruleGroups1 rules)
>     Random rSeed -> randQs (ruleGroups1 rules) (mkStdGen rSeed)

> makeQsT p rules = case initDist p of
>     Uniform -> uniQs (ruleGroups2 rules) 
>     Random rSeed -> randQs (ruleGroups2 rules) (mkStdGen rSeed) 


> mainSub1 p rules startSym allData initQs bachEnv num = do 
>     let theSeed = (dataSeeds p !! num)
>     putLogLn (logFile p) "===================" 
>     putLogLn (logFile p) ("Seed: " ++ show theSeed)
>     putLogLn (logFile p) "Starting..."
>     let bachChordsTaken = randomData (sampleSize p) theSeed allData
>     writeFile (sampleFile p theSeed) $ show bachChordsTaken
>     putLogLn (logFile p) ("Randomly taken data points: "++show (length bachChordsTaken))
>     putLogLn (logFile p) ("Average progression length: "++show(averageLength bachChordsTaken))
>     putLogLn (logFile p) ("Maximum progression length: "++show(maximum $ map length bachChordsTaken))
>     putLogLn (logFile p) ("Minimum progression length: "++show(minimum $ map length bachChordsTaken))
>     qs <- learnProbs bachEnv bachChordsTaken (runIters p) (minProb p) (convThresh p)
>     let qsAll = phis bachEnv : qs
>         prsAll = map (\q -> zip q rules) qsAll
>         prs = last prsAll
>         resultFile = makePre (confFile p) ++ outNamePre p ++ "s"++show theSeed ++".txt"
>         ruleStrs = mkQStr prs
>     putLogLn (logFile p) "Writing results..."
>     writeFile resultFile (stats theSeed p 
>                           ++ "Results: \n"++ruleStrs ++ 
>                           "\n=============\n\n" ++ mkQTable [0..(runIters p)] rules qsAll) -- allQs prsAll)
>     putLogLn (logFile p) "Done."
>     return $ last qs

> getPhi (a,b,c,d) = d

> makePre fname = 
>     let toks = splitBy '.' fname
>     in  concat $ intersperse "." $ take (length toks - 1) toks 


> stats rs p = "Program version: "++show version++
>         "\nRandom seed for data selection: "++show rs ++
>         "\nIterations (re-estimation): "++ show (runIters p) ++
>         "\nData points: "++show (sampleSize p)++" ("++show (dataMode p)++")\n\n"


CHECKSUM

> sameLHS :: (Eq a) => PRule a -> PRule a -> Bool
> sameLHS (d,(l,r)) (d',(l',r')) = l==l'

> ecOp :: (Eq a, Show a) => [PRule a] -> String
> ecOp [] = []
> ecOp es@((d,(l,r)):rest) = 
>     let check = sum $ map fst es
>     in  show l ++"'s sum: "++show check++"\n"

> checkProbs :: (Eq a, Show a) => [PRule a] -> String
> checkProbs rs = "\n\n"++concatMap ecOp (rs // sameLHS)


> showRule' :: (Show a) => (a, [a]) -> String
> showRule' (a,xs) = show a ++ " -> "++ concat (intersperse " " $ map show xs)

> showInter:: (Show a) => String -> [a] -> String
> showInter x xs = concat $ intersperse x $ map show xs

> mkQTable :: [Int] -> [RRule RTerm] -> [[Double]] -> String
> mkQTable seeds rs [] = error "(mkQTable) empty table!"
> mkQTable seeds rs qs = 
>     let qRows = Data.List.transpose qs
>         f r qr = showRule' r ++ "\t" ++ showInter "\t" qr ++ "\n"
>     in  "Rule\t" ++ showInter "\t" seeds ++ "\n" ++
>         concat (zipWith f rs qRows)

> mkQTableT :: [Int] -> [RRule (CType, Rational)] -> [[Double]] -> String
> mkQTableT seeds rs [] = error "(mkQTable) empty table!"
> mkQTableT seeds rs qs = 
>     let qRows = Data.List.transpose qs
>         f r qr = showRuleT2 r ++ "\t" ++ showInter "\t" qr ++ "\n"
>     in  "Rule\t" ++ showInter "\t" seeds ++ "\n" ++
>         concat (zipWith f rs qRows)

> showRohrStr = drop 1 . concatMap ((" "++).show)

> mkQStr = concat . zipWith (\i s -> show i ++ ".\t" ++s) [0..] . map showRule

> mkQStrT = concat . zipWith (\i s -> show i ++ ".\t" ++s) [0..] . map showRuleT1

> allQs = concat . zipWith (\i prs -> "Iteration "++show i++"\n"++mkQStr prs++"\n\n") [0..]

> showRule :: PRule RTerm -> String
> showRule (d, (l,r)) = show d ++ "\t" ++ show l ++ " -> " ++ showRohrStr r ++ "\n"

> showRuleT1 :: (Double, RRule (CType, Rational)) -> String
> showRuleT1 (d, (l,r)) = show d ++ " \t" ++ showRuleT2 (l,r)

> showRuleT2 :: RRule (CType, Rational) -> String
> showRuleT2 (l,r) = show l ++ " -> " ++ showIt r where
>     showIt = concat . intersperse "  " . map show

> randomData :: Int -> Int -> [[a]] -> [[a]]
> randomData amt seed dataIn = 
>     let f = randomR (0, length dataIn - 1) . snd
>         inds = take amt $ tail $ nub $ map fst $ iterate f (0, mkStdGen seed) 
>     in if amt <= 0 || amt >= length dataIn then dataIn 
>        else map (dataIn !!) inds


========================================
Code for parsing configuration files

> testCFile f = readFile f >>= (printParams . parseCFile f)

> splitBy :: (Eq a) => a -> [a] -> [[a]]
> splitBy a [] = []
> splitBy a str = let xs = takeWhile (/=a) str in xs : splitBy a (drop (length xs + 1) str)

> parseCFile :: String -> String -> Params
> parseCFile fname cstr = 
>     let plines = map (splitBy '\t') $ filter ((/="#").take 1) $ lines cstr 
>         goodData = and $ map ((>=2).length) plines
>     in  if not goodData then error "Badly formed configuration file!"
>         else Params {
>             confFile = fname,
>             dataFile = 	plines !! 0 !! 1,
>             gramFile = 	plines !! 1 !! 1,
>             gramMode = 	read (plines !! 2 !! 1),
>             runIters = 	read (plines !! 3 !! 1),
>             convThresh = 	read (plines !! 4 !! 1),
>             dataSeeds = 	read (plines !! 5 !! 1),
>             initDist = 	read (plines !! 6 !! 1),
>             dataMode = 	readMode (plines !! 7 !! 1),
>             maxLength = 	read (plines !! 8 !! 1),
>             sampleSize = 	read (plines !! 9 !! 1),
>             filterData = 	read (plines !! 10 !! 1),
>             outNamePre = 	plines !! 11 !! 1,
>             minProb = read (plines !! 12 !! 1),
>             logFile = makeLogFN fname }

> printParams p = 
>     putLogLn (logFile p) (show $ confFile p) >>
>     putLogLn (logFile p) (show $ dataFile p) >>
>     putLogLn (logFile p) (show $ gramFile p) >>
>     putLogLn (logFile p) (show $ gramMode p) >>
>     putLogLn (logFile p) (show $ runIters p) >>
>     putLogLn (logFile p) (show $ convThresh p) >>
>     putLogLn (logFile p) (show $ dataSeeds p) >>
>     putLogLn (logFile p) (show $ initDist p) >>
>     putLogLn (logFile p) (show $ dataMode p) >>
>     putLogLn (logFile p) (show $ maxLength p) >>
>     putLogLn (logFile p) (show $ sampleSize p) >>
>     putLogLn (logFile p) (show $ filterData p) >>
>     putLogLn (logFile p) (show $ outNamePre p) >>
>     putLogLn (logFile p) (show $ minProb p) >>
>     putLogLn (logFile p) (show $ logFile p) 

> makeLogFN :: FilePath -> FilePath
> makeLogFN fp = reverse (drop 3 $ reverse fp) ++ "log"

> readMode :: String -> [Mode]
> readMode "Major" = [Major]
> readMode "Minor" = [Minor]
> readMode "Both" = [Major, Minor]
> readMode x = error ("Badly formed mode: "++show x)


==========================================

Grammar reading

> testPCFG f = readFile f >>= (printPCFG . parsePCFG)

> splitBy2 :: (Eq a) => [a] -> [a] -> [[a]]
> splitBy2 a [] = []
> splitBy2 a str = 
>     let xs = takeWhile (not . (`elem` a)) str 
>         rest = splitBy2 a $ drop (length xs + 1) str
>     in  if null xs then rest else xs : rest

> parsePCFG :: String -> (RTerm, [RRule RTerm])
> parsePCFG gramstr = 
>     let gLines = filter (not.null) $ lines gramstr
>         gLines' = map (splitBy2 "->") gLines
>         sLines = filter ((==1).length) gLines'
>         rLines = filter ((>=2).length) gLines' 
>         startSym = if null sLines then error "(parsePCFG) No start symbol!"
>                    else read $ head $ head sLines
>         f rln = (read $ head rln, map read $ splitBy ',' $ rln !! 1)
>     in  (startSym, map f rLines)

> showRule2 :: RRule RTerm -> String
> showRule2 (l,r) = show l ++ " -> " ++ showRohrStr r ++ "\n"

> printPCFG (s, rs) = 
>     putStrLn ("Start Symbol: " ++ show s) >>
>     putStrLn (concatMap showRule2 rs)

> readPCFG :: FilePath -> IO (RTerm, [RRule RTerm])
> readPCFG fp = readFile fp >>= (return . parsePCFG)



==========================================

Simple data file parsing

> readData f = readFile f >>= (return . parseData)
> readData2 f = readFile f >>= (return . parseData2)

Old definition of parseData:

> parseData :: String -> [(Int, Mode, [CType])]
> parseData dstr = 
>     let dlines = filter (not.null) $ lines dstr
>         rlines = map (splitBy '\t') $ tail dlines
>         g = map read . splitBy ' '
>         f s = (read (s !! 0), readMode2 (s !! 1), g (s !! 2))
>     in  map f rlines

> parseData2 :: String -> [(Int, Mode, [RTerm])]
> parseData2 dstr = 
>     let dlines = filter (not.null) $ lines dstr
>         ftype = head dlines
>         rlines = map (splitBy '\t') $ tail dlines
>         g = map (readR ftype) . splitBy ' '
>         f s = (read (s !! 0), readMode2 (s !! 1), g (s !! 2))
>     in  map f rlines

> readR :: String -> String -> RTerm
> readR "Numerals" = C . read
> readR "TSD" = read

> readMode2 "Major" = Major
> readMode2 "Minor" = Minor
> readMode2 x = error ("(readMode2) Bad mode: "++show x)




==========================================

STUFF THAT NEEDS TO BE MOVED TO OTHER FILES


> iotr = [(TR, [T]), 
>         (TR, [TR, TR]),
>         (TR, [DR, T]),
>         (TR, [TR, DR]),
>
>         (DR, [DR, DR]),
>         (DR, [D]),
>         (DR, [SR, D]),
>
>         (SR, [S]),
>         (SR, [SR, SR]),
>
>         (T, [C VI]), 
>         (T, [C III]),
>         (T, [C I]),
>         (T, [C I, C II, C VI]), 
>         (T, [T,P]),
 
>         (D, [C VII]),
>         (D, [C V]),

>         (S, [C IV]),
>         (S, [C II]), 
>         (S, [C IV, C III, C IV]),

>         (P, [C IV, C I]),
>         (P, [C IV, P])]


bachChords = filter parseable $ map (map C) $ 
    let majExs = filter (\(a,b,c) -> b==mode) chordData
        justCs = map (\(a,b,c) -> c) majExs
        cs = take 10000 justCs
    in  if useDataFilter then dataFilter cs else cs

> fst3 (a,b,c) = a
> snd3 (a,b,c) = b
> thd (a,b,c) = c

 bachChords' p rules ss = filter (parseable rules ss) $ map (map C) $ 
     let okLines = filter ((`elem` dataMode p).snd3) chordData
         unfiltered = map thd okLines
         filtered = filter ((<=maxLength p).length) unfiltered
     in  if (filterData p) then filtered else unfiltered

> procChords :: [(Int, Mode, [CType])] -> Params -> [RRule RTerm] -> RTerm -> [[RTerm]]
> procChords chords p rules ss = filter (parseable rules ss) $ map (map C) $ 
>     let okLines = filter ((`elem` dataMode p).snd3) chords
>         unfiltered = map thd okLines
>         filtered = filter ((<=maxLength p).length) unfiltered
>     in  if (filterData p) then filtered else unfiltered

> procChords2 :: [(Int, Mode, [RTerm])] -> Params -> [RRule RTerm] -> RTerm -> [[RTerm]]
> procChords2 chords p rules ss = filter (parseable rules ss) $ 
>     let okLines = filter ((`elem` dataMode p).snd3) chords
>         unfiltered = map thd okLines
>         filtered = filter ((<=maxLength p).length) unfiltered
>     in  if (filterData p) then filtered else unfiltered

> parseable rules ss = (ss `elem`) . concat . last . allRowsMS 3 rules 

=================================

Code to support InsideOutside implementation that will do learning for either datatype

Default function for RHS side matching for simple CFGs (no parameters).
 
> findByRHS1 :: (Eq a) => [RRule a] -> TStr a -> [TRuleInst a]
> findByRHS1 rs xs = 
>     let rsi = zip [0..length rs-1] rs
>         rsi' = filter ((==xs).snd.snd) rsi
>     in  map (\(i, (l,r)) -> TRuleInst i l r) rsi'

Default function for rule grouping with simple CFGs.

> ruleGroups1 :: (Eq a) => [RRule a] -> [[RuleIndex]]
> ruleGroups1 rs = 
>     let rsi = zip [0..length rs-1] rs 
>     in  map (map fst) $ groupBy (\a b -> fst (snd a) == fst (snd b)) rsi

Default function for RHS matching for temporal CFGs

> findByRHS2 :: (Eq a) => [RRule (a,Rational)] -> TStr (a,Rational) -> [TRuleInst (a,Rational)]
> findByRHS2 rs xs = 
>     let xsNorm = temporalNorm xs
>         is = findIndices ((==xsNorm).snd) rs
>         rs' = map (rs !!) is
>         trs = map (makeInstance xs) $ zip is rs'
>     in  filter validInstance trs

> temporalNorm :: TStr (a, Rational) -> TStr (a, Rational)
> temporalNorm xs = 
>     let tsum = sum $ map snd xs
>     in  map (\(a,b) -> (a,b/tsum)) xs

> validInstance :: TRuleInst (a,Rational) -> Bool
> validInstance (TRuleInst id lhs rhs) =
>     let test1 = sum (map snd rhs) == snd lhs -- total durations are preserved
>         test2 = and $ map (\d -> elem d allDurs) $ snd lhs : map snd rhs -- no weird durs like 1/3
>     in  test1 && test2

The following are all possible durations we can expect to encounter in the data. The 
start symbol is assumed to be (I, 1.0) always, so durations must be <=1.0 and of the
form 1/(2^n) for n in [0,8].

> allDurs = [wn, hn, qn, en, sn, tn, 1/64, 1/128, 1/256] :: [Rational]

> makeInstance :: TStr (a, Rational) -> (RuleIndex, RRule (a, Rational)) -> TRuleInst (a, Rational) 
> makeInstance xs (id, ((l,d),r)) = TRuleInst id (l, sum $ map snd xs) xs

Default function for rule grouping with PCFGs written as pairs. Since the Rational component 
should always be 1.0 for the rules, the approach for simple CFGs will also work with PCFGs in
this form.

> ruleGroups2 :: (Eq a) => [RRule a] -> [[RuleIndex]]
> ruleGroups2 = ruleGroups1 

====================================

Reading Q-Files (for use in other modules)

> readProbsFinal :: FilePath -> IO [[Double]]
> readProbsFinal fp = do 
>     str <- readFile fp
>     let valsRaw = map (splitBy '\t') $ lines str
>         dVals = map (map read) $ map (drop 1) $ tail valsRaw
>     return dVals 

