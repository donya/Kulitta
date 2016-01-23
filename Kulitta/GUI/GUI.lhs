Kulitta Graphical Interface
Donya Quick
Last modified: 22-Jan-2016

NOTE: this version of the code uses:
- Euterpea 2.0
- UISF 0.4
- HSoM 1.0

See euterpea.com for information on installing 
Euterpea and HSoM. UISF will be installed automatically
in the process of installing those two libraries.

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
> import GUIBackend
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
> import PlayK
> import FRP.UISF.UISF
> import HSoM
> import FRP.UISF.AuxFunctions
> import IOWidgets


> programTitle = "Kulitta 2.0.1"

> channelOffset = 0 -- this is useful for some synthesizers

Data type definitions to allow the user to specify Kulitta's behavior.



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

