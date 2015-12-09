Module for generating large numbers of chorale phrases and 
short pieces with Kulitta.

> module Main where
> import EuterpeaSpecial
> import KulittaBackend
> import System.Random
> import MusicGrammars hiding (mode)

> makePiece2 g i@(Info s f gr m l k pfile) b pLen = do
>     let [gStruct, gFG] = take 2 $ splitN g
>         genVals = if chorale s then (5, qn, hn, pLen) else (5, wn, wn, (pLen*2))
>     absStructs <- makeStructure gStruct i genVals
>     theMusic <- makeMusic gFG i absStructs
>     return (procInstrs b theMusic, absStructs)

> outputFolder f pLen = "output\\corpus\\"++ show f ++ (show $ round pLen)++"\\"

> splitN g = let (g1,g2) = split g in g1 : splitN g2

> makeCorpus form pLen seed majCount minCount= do
>     let majInfo = Info Chorale form Learned Major False True majorProbsFile
>         minInfo = Info Chorale form Learned Minor False True minorProbsFile
>         infSeeds = splitN (mkStdGen seed)
>         majSeeds = take majCount infSeeds
>         minSeeds = take minCount $ drop majCount infSeeds
>     sequence $ map (makeFun majInfo pLen) $ zip majSeeds [0..]
>     sequence $ map (makeFun minInfo pLen) $ zip minSeeds [0..]
>     putStrLn "Done."

> makeFun info pLen (g,num) = do
>     let fileName = show (form info) ++ show (round pLen) ++ show (mode info) ++ show num ++ ".mid"
>     putStrLn ("Working on "++fileName++"...")
>     (mVal, _) <- makePiece2 g info True pLen
>     writeMidi (outputFolder (form info) pLen ++ fileName) mVal

> makeAll = do
>     putStrLn "====== Starting Phrase 4 ======"
>     makeCorpus Phrase 4 0 100 100
>     putStrLn "====== Starting Phrase 8 ======"
>     makeCorpus Phrase 8 1 100 100
>     putStrLn "====== Starting AABA 4 ======"
>     makeCorpus AABA 4 1 100 100
>     putStrLn "====== Starting AABA 8 ======"
>     makeCorpus AABA 8 2 100 100
>     putStrLn "====== Corpus Gen Complete ======"

> main = makeAll