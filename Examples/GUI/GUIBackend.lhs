Generative backend for Kulitta.lhs (the GUI/console program)

> module GUIBackend where
> import Kulitta.EuterpeaSpecial
> import Kulitta.Foregrounds
> import Kulitta
> import Kulitta.Grammars.MusicGrammars
> import System.Random
> import Kulitta.Learning.Learning
> import Kulitta.Learning.PCFGtoPTGG
> import Data.List
> import System.Environment
> import System.IO
> import System.Directory

Data type definitions to allow the user to specify Kulitta's behavior.

> data Style = Chorale | -- in style of JS Bach
>              JazzChorale | -- grammar, jazz chords, chorale foreground
>              WeirdChorale | -- grammar through OPT-space
>              JazzChords | -- grammar with jazz foreground
>              BossaNova -- grammar with bossa foreground
>    deriving (Eq, Show, Ord, Read)
> data Form = Phrase | AABA
>    deriving (Eq, Show, Ord, Read)
> data GramType = HandBuilt | Learned
>    deriving (Eq, Show, Ord, Read)
> data Info = Info {
>     style :: Style, 
>     form :: Form, 
>     gram :: GramType, 
>     mode :: Mode, 
>     lets :: Bool, 
>     randKey :: Bool,
>     probs :: FilePath}


ABSTRACT PHRASE GENERATION

This function creates chorale phrases using a hand-built grammar. 

> makeRPhraseH :: StdGen -> Mode -> (Dur,Dur) -> Bool -> Int -> Dur -> Bool -> IO (Constraints, [RChord])
> makeRPhraseH g m (minD,maxD) lets iters len partB= 
>     let tSeed = [NT (I, MP len m 0 0 4)]
>         tVal = doGen (rRules1 minD lets) iters g m tSeed maxD
>         kVal = findInds [] tVal
>     in  return (kVal, toChords (expand [] tVal))

This files store the results of training Kulitta on a Bach chorale corpus.

> minorProbsFile = "minorProbs.txt"
> majorProbsFile = "majorProbs.txt"
> pcfgFile = "pcfg.txt"

These production probabilities can be used to make other phrases. Since we
require that phrases end on one, phrases will be searched until one is found 
that does end on I (ending on I is not guaranteed by the learned grammar).
 
> makeRPhraseB :: StdGen -> Mode ->(Dur,Dur) -> Int -> Dur -> Bool -> FilePath -> IO (Constraints, [RChord])
> makeRPhraseB g m (minD,maxD) iters len partB pfile = do
>     let [g1, g2, g'] = take 3 $ splitN g
>     (startSym, rulesPCFG) <- readPCFG pcfgFile
>     let theFile = if null pfile then
>                       if m==Major then majorProbsFile else minorProbsFile
>                   else pfile
>     probs <- readProbsFinal theFile
>     let avgProbs = map average probs
>         prules = zip avgProbs rulesPCFG
>         rules = toPTGG3 (<minD) prules
>         tSeed = [NT (I, MP len m 0 0 4)]
>         tPhrase = doGen rules iters g1 m tSeed maxD
>         endsOnI = endingType tPhrase
>         rPhrase = toChords (expand [] $ tPhrase)
>         rPhrase' = expandTSD2 (tsdSpace' m) (okRTrans m) g2 rPhrase
>     putStrLn ("Probabilities: "++show avgProbs)
>     if endsOnI then return ([], rPhrase') else makeRPhraseB g' m (minD,maxD) iters len partB pfile where
>     endingType = (==I) . last . map (\(a,b,c) -> c) . toChords . expand []

> average xs = sum xs / fromIntegral (length xs)


The doGen function adds a maximum duration constraint to get a more consistent 
distribution of durations. No more than searchLimit generative steps will be tried
to avoid infinite loops caused by generative fixed points.

> searchLimit = 1000

> doGen :: (Eq a) => [Rule a MP] -> Int -> StdGen -> Mode -> Sentence a MP -> Dur -> Sentence a MP
> doGen theRules i g ctxt t maxDur = recCheck i (map snd $ gen theRules (g,t)) where
>     recCheck i ts = let tc = ts !! i in
>         if (goodDurs tc && goodDurs' tc) || i>searchLimit then tc else recCheck (i+1) ts
>     goodDurs [] = True
>     goodDurs (Let x a e : ts) = goodDurs a && goodDurs e && goodDurs ts
>     goodDurs (Var x : ts) = True -- can't tell duration here, so assume ok
>     goodDurs (NT (c,p) : ts) = dur p <= maxDur && goodDurs ts

> goodDurs' :: Predicate (Sentence a MP)
> goodDurs' t = let x = getDurs t in 
>     length (filter (>=hn) x) < length x `div` 2

> getDurs :: Sentence a MP -> [Dur]
> getDurs [] = []
> getDurs (t@(Let x a e):ts) = getDurs (expand [] [t]) ++ getDurs ts
> getDurs (Var x : ts) = error "getDurs can't handle variables."
> getDurs (NT (x,p) :ts) = dur p : getDurs ts

This version of TSD-space is altered from the originally-learned statistics to
improve Kulitta's performance (the analysis had some known problems). 

> tsdSpace' :: Mode -> QSpace CType
> tsdSpace' m = [f I 0 ++ f III 2 ++ f VI 5,
>                f IV 3 ++ f II 1,
>                f V 4 ++ f VII 6] where
>                             --  I   II  III  IV   V   VI  VII
>     rCounts = if m==Major then [34, 13,  4,  11,  25,  7,  3]
>               else             [47,  2,  4,  18,  20,  3,  3]
>     f x i = take (rCounts !! i) $ repeat x


===============================

FOREGROUND GENERATION

The following code configures Kulitta in different ways to generate different 
styles of music according to the user's specifications.

> makePiece g i@(Info s f gr m l k pfile) b = do
>     let [gStruct, gFG] = take 2 $ splitN g
>         genVals = if chorale s then (5, qn, hn, 4) else (5, wn, wn, 8)
>     absStructs <- makeStructure gStruct i genVals
>     theMusic <- makeMusic gFG i absStructs
>     return (procInstrs b theMusic, absStructs)

> procInstrs :: Bool -> Music a -> Music a
> procInstrs True m = m
> procInstrs False (m1 :=: m2) = procInstrs False m1 :=: procInstrs False m2
> procInstrs False (m1 :+: m2) = procInstrs False m1 :+: procInstrs False m2
> procInstrs False (Modify (Instrument i) m) = procInstrs False m
> procInstrs False m = m

> makeSubStruct gAbs i@(Info s f gr m l k pfile) (iters, minD, maxD, len) partB = do
>    (cons, abs) <- if gr==HandBuilt then makeRPhraseH gAbs m (minD,maxD) l iters len partB else
>                   makeRPhraseB gAbs m (minD,maxD) (iters+1) len partB pfile
>    return (cons, abs)      

> makeStructure gStruct i@(Info s f gr m l k pfile) genVals = do
>     let (g1, g2) = split gStruct
>     structs <- if f==Phrase then sequence [makeSubStruct g1 i genVals False]
>                else sequence [makeSubStruct g1 i genVals False, 
>                               makeSubStruct g2 i genVals True]
>     return structs

> chorale s = elem s [Chorale, JazzChorale, WeirdChorale]

> makeMusic g i@(Info s f gr m l k1 pfile) absStructs = do
>     let (k2, gFG) = randomR (0,11::Int) g
>         k = if k1 then k2 else 0
>     let fg = case s of 
>                  Chorale -> addVolume 127 $ buildChorale gFG absStructs (k,m)
>                  JazzChorale -> addVolume 127 $ buildJChorale gFG absStructs (k,m)
>                  WeirdChorale -> addVolume 127 $ buildWChorale gFG absStructs (k,m) 
>                  JazzChords -> addVolume 127 $ buildJazzChords gFG absStructs (k,m)
>                  BossaNova -> buildBossaNova gFG absStructs (k,m)
>     putStrLn ("Key of piece: "++ showKey k m ++"\n")
>     writeFile "term.txt" (show absStructs)
>     return fg where
>     showKey k m = (["C","C-sharp","D","E-flat","E","F","F-sharp","G",
>                     "A-flat","A","B-flat","B"] !! k) ++ " " ++ show m

A chorale is pretty straightforward, using the ClassicalFG.lhs implementation.

> 
> buildChorale g [(cons, x)] (k,m) = 
>     snd $ snd $ classicalFGR g (ctTrans k x) cons
> buildChorale g [(cons1, a), (cons2, b)] (k,m) = 
>     let [g1, g2, g3, g4, g5] = take 5 $ splitN g
>         aChords = snd $ classicalCS g1 (ctTrans k a) cons1
>         bChords = snd $ classicalCS g2 (ctTrans k b) cons2
>         partA = snd $ snd $ classicalFG' g3 aChords
>         partA' = snd $ snd $ classicalFG' g4 aChords
>         partB = snd $ snd $ classicalFG' g5 bChords
>     in  partA :+: partA' :+: partB :+: partA

A "jazz chorale" ads an extra step in the foreground generation, converting 
numerals to jazz chords before running the classical algorithms.

> buildJChorale g [(cons, x)] (k,m) =  
>     let (g1,g2) = split g
>         jChords = atTrans k $ snd $ jazzChords g1 x cons
>     in  snd $ snd $ classicalFG' g2 jChords
> buildJChorale g [(cons1, a), (cons2, b)] (k,m) = 
>     let [g1, g2, g3, g4, g5] = take 5 $ splitN g
>         aChords = atTrans k $ snd $ jazzChords g1 a cons1
>         bChords = atTrans k $ snd $ jazzChords g2 b cons2
>         partA = snd $ snd $ classicalFG' g3 aChords
>         partA' = snd $ snd $ classicalFG' g4 aChords
>         partB = snd $ snd $ classicalFG' g5 bChords
>     in  partA :+: partA' :+: partB :+: partA


A "weird chorale" is one where numerals are run through OPTC-space before
applying a classical foreground.

> qOPTC = satbR (mkStdGen 123) satbFilter2 optcEq 

Note: the key will not affect weird chorales due to the use 
of OPTC-equivalence.

> buildWChorale g [(cons, x)] km = 
>     let (g1, g2) = split g
>         optChords = toOPTC g1 x km
>     in  snd $ snd $ classicalFG' g2 optChords
> buildWChorale g [(cons1, a), (cons2, b)] km = 
>     let [g1, g2, g3, g4, g5] = take 5 $ splitN g
>         aChords = toOPTC g1 a km
>         bChords = toOPTC g2 b km
>         partA = snd $ snd $ classicalFG' g3 aChords
>         partA' = snd $ snd $ classicalFG' g4 aChords
>         partB = snd $ snd $ classicalFG' g5 bChords
>     in  partA :+: partA' :+: partB :+: partA

> toOPTC g x (k,m) = 
>     let aChords = atTrans k $ map toAbsChord x
>         es = map (eqClass qOPTC optcEq) $ map thd aChords
>     in  zipWith newP aChords $ greedyProg' vl7 nearFall g es

Jazz foregrounds are created using the two algorithms in JazzFG.lhs.

> buildJazzChords = buildJazz jazzFG1
> buildBossaNova = buildJazz jazzFG2

> buildJazz f g [(cons, x)] (k,m) = snd $ f g (ctTrans k x) []
> buildJazz f g [(cons1, a), (cons2, b)] (k,m) = 
>     let [g1,g2,g3] = take 3 $ splitN g
>         jA = snd $ f g1 (ctTrans k a) []
>         jA' = snd $ f g2 (ctTrans k a) []
>         jB = snd $ f g3 (ctTrans k b) []
>     in  jA :+: jA' :+: jB :+: jA



=======================================

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