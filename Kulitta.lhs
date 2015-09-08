Kulitta Program 2.0.1
Donya Quick
Last modified: 08-September-2015

NOTE: this version of the code uses Euterpea 1.1 and UISF 4.0 
on Hackage. It will NOT work with Euterpea 1.0 or UISF <4. 
If you have older versions of Euterpea already installed, you
can update them by doing this:

cabal update
cabal install Euterpea

If you get warnings about reinstalling, you can use this:

cabal install Euterpea --reinstall --force-reinstalls

Changes since last version:
- Compatibility fixes to work with most recent Euterpea/UISF
- Now writes a .txt file with the Haskell value of the 
  abstract musical structure(s) of the piece generated. 
  If you write foo.mid, it will produce foo.txt as well.

-------------------------------------------

This module provides examples of Kulitta's output using an
interactive interface. The program can be compiled by:

ghc -O2 Kulitta.lhs

Running "Kulitta" will start the program in GUI/MUI mode. 
The program will also respond to different arguments:

Kulitta help		Explains how to use the program.
Kulitta about		More information about the program.
Kulitta basic		Interactive, command-line version. 

To run Kulitta in GHCI, you can load this file and run the
"main" function directly, starting the progam in GUI mode.
However, you will not be able to use the argument-based 
methods of interaction through GHCI. If you would like to
use enter the parameters using a text-based series of 
prompts in GHCI, change the contents of "settings.txt" to  
be "basic" instead of "mui" (which is the default). 

-------------------------------------------

> {-# LANGUAGE Arrows #-}

> module Main where
> import EuterpeaSpecial
> import JazzFG
> import ClassicalFG
> import PTGG 
> import MusicGrammars
> import System.Random
> import PostProc
> import Learning
> import PCFGtoPTGG
> import ChordSpaces hiding (i)
> import Data.List
> import System.Environment
> import System.IO
> import System.Directory
> import BachStat
> import Euterpea.Experimental
> import Euterpea.ExperimentalPlay
> import PlayK
> import Search
> import Constraints
> import FRP.UISF.UISF
> import IOWidgets


> programTitle = "Kulitta 2.0.1"

> channelOffset = 0 -- this is useful for some synthesizers

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

=======================

MAIN PROGRAM DEFINITION

Main program description. There are two modes: interacive and automated with arugments.

> main = do
>     hSetBuffering stdout NoBuffering -- required to make things print in the right order
>     putStrLn ("\n\n===== "++ programTitle ++ "=====\n")
>     args <- getArgs
>     if length args == 0 then putStrLn "\nHello! The graphical interface is now active. \n" >> 
>                              runDefault else 
>         if args !! 0 == "basic" then runBasicVersion else
>         if args !! 0 == "help" then printDirections else
>         if args !! 0 == "about" then printAbout else
>         if length args < 7 then putStrLn "\nSorry, I couldn't understand those arguments.\n" >>
>                                 printDirections
>         else processArgs args

> runDefault = do
>    files <- getDirectoryContents ""
>    if not (elem "settings.txt" files) then putStrLn "File settings.txt not found." >> runMuiVersion else do
>         startMode <- readFile "settings.txt"
>         if take 5 startMode == "basic" then runBasicVersion else runMuiVersion

> runMuiVersion = mui >> putStrLn "\nGraphical interface closed. Goodbye!\n\n" 
> runBasicVersion = interactive >> putStrLn "\nGoodbye!\n\n"

=======================

MUI DEFINITION

> styles = [Chorale, JazzChorale, WeirdChorale, JazzChords, BossaNova]
> forms = [Phrase, AABA]
> grams = [HandBuilt, Learned]
> modes = [Major, Minor]

> mui = runMUI defaultMUIParams{uiSize=(500,570), uiTitle=(programTitle++" - Graphical Interface")} $ proc _ -> do
>   label "Please specifty composition parameters for Kulitta." -< ()
>   label "The command prompt will show the generation/playback status." -< ()
>   label "" -< ()
>   (style', mo) <- inputPaneA -< ()
>   (form',gram',mode', instr', let', key') <- inputPaneB -< ()
>   probsFile <- leftRight $ label "Prob. file:  " >>> textbox "" -< Nothing
>   seed <- leftRight $ seedPanel -< ()
>   let fileName = "output\\" ++ show (styles !! style') ++ "_" ++ show seed ++ ".mid"
>   fileName' <- unique -< fileName
>   outFile <- leftRight $ label "Output File: " >>> textbox "test.mid" -< fileName' 
>   volStr<- leftRight $ label "Playback volume (0.0 to 1.0): " >>> textbox "1.0" -< Nothing
>   let s = styles !! style'
>       f = forms !! form'
>       g = grams !! gram'
>       m = modes !! mode'
>       iMIDI = instr'==0
>       iLet = let'==0
>       iKey = key'==0
>       iVal = Info s f g m iLet iKey probsFile
>       vol0 = reads volStr
>       vol = if null vol0 then 1.0 else fst $ head vol0
>   (g,p) <- buttons -< ()
>   let g' = fmap (const (iVal, seed, outFile, iMIDI)) g
>       p' = fmap (const (outFile, mo, vol)) p
>   basicIOWidget genWrap -< g'
>   basicIOWidget playWrap -< p'
>   returnA -< () 

> seedPanel :: UISF () Int
> seedPanel = proc _ -> do
>   rec seedT <- leftRight $ label "Random seed: " >>> textbox "" -< x
>       b <- edge <<< button "Random!" -< () -- button to automatically get a random number
>       x <- ioWidget2 Nothing (const rFun) -< b
>       let seed = let x = reads seedT :: [(Int, String)] -- parse the string
>                  in  if null x then 0 else fst $ head x 
>   returnA -< seed where
>     rFun :: IO (Maybe String)
>     rFun = do
>         x <- randomIO :: IO Int -- fetch a random number
>         return (Just $ show $ abs x) -- convert it to SEvent String format

> genWrap (i, seed, outFile, inst) = automated i seed outFile inst
> playWrap (fname, devID, vol) = do
>    putStrLn "\nPlaying...(please wait)\n" 
>    playX fname devID channelOffset vol 
>    putStrLn "\nDone!\n\n"

> buttons = leftRight $ proc _ -> do
>   genButton <- edge <<< button "Generate MIDI File" -< ()
>   playButton <- edge <<< button "Play MIDI File" -< ()
>   returnA -< (genButton, playButton)

> inputPaneA = leftRight $ proc _ -> do
>   style <- topDown $ title "Style" $ radio (map show styles) 0 -< ()
>   mo <- topDown $ selectOutput -< ()
>   returnA -< (style, mo)

> inputPaneB = leftRight $ proc _ -> do
>   (form, gram, lets) <- inputPane2 -< ()
>   (mode, instr, key) <- inputPane3 -< ()
>   returnA -< (form, gram, mode, instr, lets, key) where
>     inputPane2 = topDown $ proc _ -> do
>       form <- topDown $ title "Form" $ radio (map show forms) 0 -< ()
>       gram <- topDown $ title "Harmony Model" $ radio (map show grams) 0 -< ()
>       lets <- topDown $ title "Use Lets" $ radio ["Yes", "No"] 1 -< ()
>       returnA -< (form, gram, lets)
>     inputPane3 = topDown $ proc _ -> do
>       mode <- topDown $ title "Mode" $ radio ["Major", "Minor"] 0 -< ()
>       instr <- topDown $ title "Assign MIDI instruments?" $ radio ["Yes", "No"] 0 -< ()
>       key <- topDown $ title "Random key?" $ radio ["Yes", "No"] 0 -< ()
>       returnA -< (mode, instr, key)

> basicIOWidget :: (a -> IO ()) -> UISF (SEvent a) ()
> basicIOWidget = (>>> arr (const ())) . uisfSinkE



=======================

CONSOLE PROGRAM DEFINITION

> printAbout = do
>     putStrLn "Created by Donya Quick at Yale University (donya.quick@yale.edu)" 
>     putStrLn "For more information, go to http://www.donyaquick.com and click on "
>     putStrLn "Current Research. Relevant publications can be found on the "
>     putStrLn "Yale Haskell Group's website, http://haskell.cs.yale.edu."

> printDirections = do
>     putStrLn "To call Kulitta with a graphical interface, just run 'Kulitta' (no arguments)."
>     putStrLn "To use Kulitta from the command prompt, run 'Kulitta basic' and follow the prompts.\n"
>     putStrLn "To provide arguments, use 'Kulitta s f m g i b x' where"
>     putStrLn "  s = Chorale | JazzChorale | WeirdChorale | JazzChords | BossaNova"
>     putStrLn "  f = Phrase | AABA"
>     putStrLn "  m = Major | Minor"
>     putStrLn "  g = HandBuilt | Learned"
>     putStrLn "  i = an integer, like 392"
>     putStrLn "  b (assign MIDI instruments) = Yes | No"
>     putStrLn "  x = a file path, like 'foo.mid'\n"
>     putStrLn "Run 'Kulitta about' for more information on the program."
>     putStrLn "You can use Ctrl+C to exit at any time while Kulitta is running.\n\n"

> processArgs strs = do
>     let style = read (strs !! 0)
>         form = read (strs !! 1)
>         mode = read (strs !! 2)
>         gram = read (strs !! 3)
>         seed = read (strs !! 4) :: Int
>         inst = strs !! 5 == "Yes" || strs !! 5 == "yes"
>         outFile = strs !! 6
>     automated (Info style form gram mode False True "") seed outFile inst

> interactive = do 
>     style <- getStyle
>     form <- getForm
>     mode <- getMode
>     gram <- getGram
>     seed <- getSeed
>     inst <- getInstr
>     useLets <- getLets (gram == HandBuilt)
>     randomKey <- getKey
>     outFile <- getFilePath
>     automated (Info style form gram mode useLets True "") seed outFile inst

> automated (Info style form gram mode lets key pfile) seed outFile inst = do
>     putStrLn ("\nI will now write a "++ show mode ++ " "++ show style ++
>               " in "++show form++" form with random seed "++ show seed ++ 
>               " and a "++show gram++" grammar, and I will "++
>               " and write it to the file '"++ outFile ++"'.\n")
>     putStrLn "Please be patient - some styles can take a while to write!\n"
>     putStrLn "Working...\n"
>     (m,abst) <- makePiece (mkStdGen seed) (Info style form gram mode lets key pfile) inst
>     writeMidi outFile m
>     let outFile2 = take (length outFile - 3) outFile ++ "txt"
>     writeFile outFile2 (show abst)
>     putStrLn ("Done! Please check "++outFile++" to hear what I wrote.\n")

> getStyle = do
>   putStrLn ("I can write the following styles:\n Chorale \t JazzChorale "++
>              "\t WeirdChorale \n JazzChords \t BossaNova ")
>   putStrLn "\nWhat would you like me to write?\n"
>   putStr "Style: "
>   styleStr <- getLine
>   let style = reads styleStr :: [(Style, String)]
>   if null style then putStrLn ("\nSorry, I don't understand. "++
>                             "Please type the style exactly. ") >> getStyle 
>       else return (fst $ head $ style)

> getForm = getOne [Phrase, AABA]
>   "\nI can write two forms: Phrase or AABA. Which would you like?\n"
>   "Form: "

> getMode = getOne [Major, Minor]
>   "\nShould this be a Major or a Minor piece?\n"
>   "Mode: "

> getSeed = do
>   putStr "\nGive me a random number seed to use: "
>   seedStr <- getLine
>   let seed = reads seedStr :: [(Int, String)]
>   if null seed then putStr ("\nSorry, I don't understand. "++
>                            "Please enter an integer (Int) value. ") >> getSeed
>       else return (fst $ head $ seed)

> getGram = getOne [HandBuilt, Learned] 
>     "\nShould I use the HandBuilt or Learned grammar for harmony? "
>     "Grammar: "

> getFilePath = do
>   putStrLn "\nWhere would you like me to write the MIDI file and what should I call it?\n"
>   putStr "Output file path: "
>   filePath <- getLine
>   return $ stripQuotes filePath

> getInstr = getYesNo "\nShould I assign MIDI instruments?\n"

> getLets hb = if not hb then return False else 
>     getYesNo "\nShould I use Let statements?\n"

> getKey = getYesNo "\nShould I pick a random key? (C is the default)\n"

> getYesNo :: String -> IO Bool
> getYesNo q = do
>   putStrLn q
>   putStr "Yes or No: "
>   ansStr <- getLine
>   let ans = if ansStr=="Yes" || ansStr=="yes" then [True] else
>              if ansStr=="No" || ansStr=="no" then [False] else []
>   if null ans then putStr ("\nSorry, I don't understand. "++
>                     "Please type only 'Yes' or 'No'. ") >> getYesNo q
>       else return (head ans) 

> getOne :: (Show a) => [a] -> String -> String -> IO a
> getOne opts q s = do
>   putStrLn q
>   putStr s
>   ansStr <- getLine
>   let ansInd = findIndices ((==ansStr).show) opts
>   if null ansInd then putStr ("\nSorry, I don't understand. "++
>                     "Please type only "++optsStr opts++". ") >> getOne opts q s
>       else return (opts !! head ansInd) where
>   optsStr :: (Show a) => [a] -> String
>   optsStr [] = ""
>   optsStr [x] = " or "++show x
>   optsStr (x:xs) = show x ++", "++optsStr xs


> stripQuotes = filter (not . (`elem` "\"'"))

===============================

ABSTRACT PHRASE GENERATION

This function creates chorale phrases using a hand-built grammar. 

> makeRPhraseH :: StdGen -> Mode -> (Dur,Dur) -> Bool -> Int -> Dur -> Bool -> IO (Constraints, [RChord])
> makeRPhraseH g m (minD,maxD) lets iters len partB= 
>     let tSeed = [NT (I, MP len m 0)]
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
>         tSeed = [NT (I, MP len m 0)]
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