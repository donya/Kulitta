Kulitta Algorithmic CompositionExample 
Donya Quick

Composed in January, 2015

Last modified 01-July-2015 to remove old/unused lines of code and update
the comments accordingly.

The complete composition titled "Vesicularia" is represented by the value 
called "vesicularia" later in this file. However, this is not how it was 
rendered to audio to produce the final recording. Instead, individual parts
were exported to MIDI individually for easier manipulation within a DAW 
(although the overall structure is still the same as in the Music value 
called "vesicularia").

> module Vesicularia where
> import ChordSpaces
> import Constraints
> import PTGG
> import MusicGrammars
> import System.Random
> import Data.List
> import EuterpeaSpecial hiding (line1)
> import Euterpea.ExperimentalPlay
> import PostProc
> import qualified Euterpea as Eu
> import ClassicalFG
> import JazzFG
> import Search

Disambiguations:

> etrans = EuterpeaSpecial.transpose

First we will define a new alphabet. Unlike the other
grammars Kulitta has, this one will not use chords.
Instead, it will an alphabet of general direction of 
motion (up or down) and the parameter will be the 
amount of the motion as an integer. This "amount" 
will have to be interpreted later.

> data Motion = Down | Up
>     deriving (Eq, Show, Ord)

> moRules :: [Rule Motion (Int,Dur)]
> moRules = [
>     (Up, 0.2) :-> \(a,t) -> [NT (Up, (a,t))], -- ID rule
>     (Up, 0.2) :-> \(a,t) -> [NT (Up, (abs'(a-1), t/3)), NT (Up, (a+1, 2*t/3))],
>     (Up, 0.2) :-> \(a,t) -> [NT (Up, (abs'(a-1), 2*t/3)), NT (Down, (a+1, t/3))],
>     (Up, 0.2) :-> \(a,t) -> [NT (Up, (a+1, t/3)), NT (Up, (abs'(a-1), 2*t/3))],
>     (Up, 0.2) :-> \(a,t) -> [NT (Up, (a+1, 2*t/3)), NT (Down, (abs'(a-1), t/3))],
>     (Down, 0.2) :-> \(a,t) -> [NT (Down, (a,t))], -- ID rule
>     (Down, 0.2) :-> \(a,t) -> [NT (Down, (abs'(a-1), t/3)), NT (Down, (a+1, 2*t/3))],
>     (Down, 0.2) :-> \(a,t) -> [NT (Down, (abs'(a-1), 2*t/3)), NT (Up, (a+1, t/3))],
>     (Down, 0.2) :-> \(a,t) -> [NT (Down, (a+1, t/3)), NT (Down, (abs'(a-1), 2*t/3))],
>     (Down, 0.2) :-> \(a,t) -> [NT (Down, (a+1, 2*t/3)), NT (Up, (abs'(a-1), t/3))]
>     ]

To interpret the motion, we will do it relative to a starting pitch.
For example, if we are starting from pitch 0 and see (Up,4), we will
have the pitch series [0,4].

> moInterp :: AbsPitch -> [(Motion, Int)] -> [AbsPitch]
> moInterp refP [] = [refP]
> moInterp refP ((Up, a):xs) = refP : moInterp (refP+a) xs
> moInterp refP ((Down, a):xs) = refP : moInterp (refP-a) xs

But what if we want to interpret these intervals a bit differently?
Lets make a scale transformation that allows interpretation as 
indices into a particular tonal system. We'll let the repeating 
unit of the scale (e.g. Western major repeats over 12 halfsteps) be 
variable.

> toScale :: AbsPitch -> [AbsPitch] -> [AbsPitch] -> [AbsPitch]
> toScale base scale = map f where
>     f x = 
>         let (xOct, xClass) = divMod x (length scale)
>         in  (scale !! xClass) + (xOct * base)

We'll just use stochastic durations here. The following is a function that 
chooses uniformly at random from a list of values to create an 
infinite list of those values. 

> infVals vals seed = recVals vals $ mkStdGen seed where
>     recVals vals g = 
>         let (i,g') = randomR (0, length vals -1) g
>         in  (vals !! i) : recVals vals g'

> toMusic :: [AbsPitch] -> [Dur] -> Music Pitch
> toMusic ps ds = line $ zipWith (\p d -> note d $ pitch p) ps ds

> type ScaleInfo = (AbsPitch, [AbsPitch])

> moGen2 :: Int -> Int -> Dur -> [(AbsPitch,Dur)]
> moGen2 s i d = 
>     let pds = toPairs $ snd $ gen moRules (mkStdGen s, [NT (Up,(1,d))]) !! i
>         f (a,(b,d)) = ((a,b),d)
>     in  applyFst (moInterp 0) $ map f pds

> mkUpDownMusic :: Dur -> AbsPitch -> ScaleInfo -> Int -> Int -> Int -> Music Pitch
> mkUpDownMusic sDur rPitch (base, scale) seed1 seed2 iters = etrans rPitch $ 
>     (uncurry toMusic) $ unzip $ applyFst (toScale base scale) $ moGen2 seed1 iters sDur

> penta = (12, [0,2,4,7,9]) :: ScaleInfo
> pentaMinor = (12, [0,3,5,7,10]) :: ScaleInfo

> lineS = pentaMinor
> line1 = mkUpDownMusic 8 60 lineS 1404 234 4
> line2 = mkUpDownMusic 8 67 lineS 1401 2345 4
> line3 = mkUpDownMusic 8 60 lineS 1123 2456 4
> line4 = mkUpDownMusic 8 67 lineS 404 34 4

> part1 = line1 :=:
>         (rest 4 :+: line2) :=:
>         (rest 8 :+: line3) :=:
>         (rest 12 :+: line4) 

> part1r = revM part1

> line5 = mkUpDownMusic 8.5 53 lineS 623 263 4
> line6 = mkUpDownMusic 8.5 55 lineS 6437 602 4
> line7 = mkUpDownMusic 8.5 50 lineS 95 8987 4
> line8 = mkUpDownMusic 8.5 48 lineS 16774 840 4

> part2 = etrans (-12) $ line5 :=:
>         (rest 4 :+: line6) :=:
>         (rest 8 :+: line7) :=:
>         (rest 12 :+: line8) 



=====================================

Now using chord-based PTGGs

A modified version of the PTGG from MusicGrammars.lhs

> rRules2 :: Dur -> Bool -> [Rule CType MP]
> rRules2 minDur useLets = normalize $ map (toRelDur2 (<minDur)) ([
>     -- Rules for I --
>     (I, 0.187) :-> \p -> [(if isMaj p then ii else iv) (q p), v (q p), i (h p)],
>     (I, 0.187) :-> \p -> map ($ q p) [i, iv, v, i],
>     (I, 0.187) :-> \p -> [v (h p), i (h p)],
>     (I, 0.187) :-> \p -> map ($ q p) $ [i, if isMaj p then ii else iv, v, i],
>     (I, 0.252) :-> \p -> [i p],
>     -- Rules for II --
>     (II, 0.50) :-> \p -> map ($ h p) $ if isMaj p then [ii, iv] else [iv, v],
>     (II, 0.50) :-> \p -> map ($ h p) $ if isMaj p then [vi, ii] else [vi, iv],
>     -- Rules for III--
>     --(III, 0.90) :-> \p -> [v (h p), iii (h p)],
>     (III, 1.0) :-> \p -> [i $ m3 (h p), v (h p)],
>     -- Rules for IV -- 
>     (IV, 0.90) :-> \p -> [v (h p), iv (h p)],
>     (IV, 0.10) :-> \p -> [i $ m4 (h p), v (h p)],
>     -- Rules for V --
>     (V, 0.10) :-> \p -> [v p],
>     (V, 0.20) :-> \p -> [iv (h p), v (h p)],
>     (V, 0.20) :-> \p -> [vi (h p), v (h p)],
>     (V, 0.10) :-> \p -> [iii p],
>     (V, 0.20) :-> \p -> [v (h p), v (h p)],
>     (V, 0.10) :-> \p -> [vii (h p), v (h p)],
>     (V, 0.10) :-> \p -> [i $ m5 p],
>     -- Rules for VI --
>     (VI, 0.70) :-> \p -> [vi p],
>     (VI, 0.30) :-> \p -> [i $ m6 p],
>     -- Rules for VII --
>     (VII, 0.50) :-> \p -> if isMaj p then [ii p] else [vii p],
>     (VII, 0.50) :-> \p -> [v (h p), vii (h p)]
>     ] ++ if useLets then letRules else []) where
>     letRules = concatMap (\ct -> [letRule1 ct, letRule2 ct]) (enumFrom I)
>     letRule1 ct = (ct, 0.05) :-> \p -> [Let "x" [NT(ct, h p)] [Var "x", Var "x"]]
>     letRule2 ct = (ct, 0.05) :-> \p -> [Let "x" [NT(ct, q p)] [Var "x", v (h p), Var "x"]]

> rGenA :: Int -> Int -> Sentence CType MP
> rGenA s i = snd $ gen (rRules2 qn False) (mkStdGen s, [NT (I,MP 4 Minor 0)]) !! i

> rGenB :: Sentence CType MP -> Music Pitch
> rGenB t = etrans 60 $ vsToMusic $ toVoices $ toAbsChords t

Let's make a simple progression and put it through a basic chord space:

> t1 = rGenA 5 6 ++ rGenA 10 6
> ta1 = toAbsChords t1
> tc1 = findInds [] t1

> (gta1,ta1') = classicalCS2 (mkStdGen 6) ta1 tc1

> randP :: (Eq a) => a -> StdGen -> [a] -> (StdGen, [a])
> randP pLast g xs = 
>     let ps = filter (\x -> head x /= pLast) $ permutations xs
>         (i,g') = randomR (0, length ps - 1) g
>     in  (g', ps !! i)

> repChords :: Dur -> [TChord] -> [TChord]
> repChords dm [] = []
> repChords dm (x@(k,d,a):xs) = 
>     if d<=dm then x : repChords d xs
>     else (k,dm,a) : repChords dm ((k,d-dm,a):xs)

> arpFG :: AbsPitch -> StdGen -> [TChord] -> Music Pitch
> arpFG pLast g [] = rest 0
> arpFG pLast g (x@(k,d,a):xs) = 
>     let (g', a') = randP pLast g a
>         n = length a
>         f p = note (d/fromIntegral n) (pitch p)
>         pLast' = last a'
>     in  line (map f a') :+: arpFG pLast' g' xs

> t1r = [(50,62), (55,67), (60,72), (65,77)]
> t1r2 = map (\(l,r) -> (l-6,r-6)) t1r
> t1arp1 = tempo 0.25 $ 
>     arpFG (-1) (mkStdGen 55) $ repChords qn $ snd $ classicalCS2' satbFilter t1r (mkStdGen 342) ta1 tc1

> t1chords = snd $ classicalCS2' satbFilter t1r2 (mkStdGen 3923) ta1 tc1
> t1chordsM = vsToMusic $ toVoices t1chords
> t1chordsFG = tempo 0.25 $ vsToMusic $ snd $ addFG defConsts (mkStdGen 234) $ toVoices t1chords

> x = instrument AcousticGrandPiano t1arp1 :=: instrument StringEnsemble1 t1chordsFG

====================================

Now some stuff with jazz spaces

> alter2nd :: (b -> d) -> (a,b,c) -> (a,d,c)
> alter2nd f (a,b,c) = (a, f b, c)

> to6 = map (\(a,b,c) -> (a,b,head c : c))

> t1' = snd $ jazzChords (mkStdGen 645) (toChords t1) []
> t1arp2 = arpFG (-1) (mkStdGen 56) $ to6 $ repChords hn $ map (alter2nd (*4)) t1'

> t1Bass = tempo 0.25 $ vsToMusic $ map tieAll $ toVoices $ snd $ 
>     classicalCS2' (const True) [(30,50)] (mkStdGen 397) (map (\(a,b,c) -> (a,b,[head c])) ta1) tc1

> t1chords2 = snd $ classicalCS2 (mkStdGen 397) ta1 tc1
> t1chordsFG2 = tempo 0.25 $ vsToMusic $ snd $ addFG defConsts (mkStdGen 845) $ toVoices t1chords

> tieAll :: Voice -> Voice
> tieAll ((a,b,c):(d,e,f):xs) = 
>     if c==f then tieAll ((a,b+e,c):xs) else (a,b,c) : tieAll ((d,e,f):xs)
> tieAll x = x



====================================

Putting it all together

Note: this will not sound entirely like the mp3,
since there is no good MIDI equivalent for the 
analog synthesizer used and also there are no
tempo or dynamic changes except for the ending.
Dynamics were added by hand in a DAW for the mp3.

> arp1 = Marimba
> arp2 = Vibraphone
> pad1 = StringEnsemble1
> pad2 = StringEnsemble2
> analogSub = Bassoon

> pad1Part = instrument pad1 (
>     part1 :=:
>     (rest (21 + qn) :+: part1r) :=: 
>     (rest 42 :+: line8) :=:
>     (rest 59 :+: line6) :=:
>     (rest 72 :+: etrans 12 line6) :=:
>     (rest 91 :+: line6) :=:
>     (rest 104 :+: etrans 12 line6))

> analogPart = instrument analogSub (
>     (rest 21 :+: part2) :=:
>     (rest 32 :+: line1) :=: 
>     (rest 38 :+: line2))

> arpPart1 = instrument arp1 (
>     (rest 51 :+: t1arp1) :=:
>     (rest 83 :+: t1arp1))

> pad2Part = instrument pad2 (
>     (rest 51 :+: t1chordsFG) :=:
>     (rest 83 :+: t1chordsFG2))

> arpPart2 = instrument arp2 (
>     (rest 83 :+: t1arp2))

> end1 = filterZeros $ dropM 30 t1arp1
> end2 = filterZeros $ dropM 30 t1arp2

> ending = Modify (Eu.Phrase [Dyn $ Diminuendo 1.0]) $
>     timesM 8 (instrument arp1 end1 :=: instrument arp2 end2)

> vesicularia = filterZeros
>     (pad1Part :=:
>     analogPart :=:
>     arpPart1 :=:
>     pad2Part :=:
>     arpPart2) :+: ending


====================================

Output of sections for rendering in a DAW:

> outputAll = do
>     writeMidi "vesicularia\\line1.mid" line1
>     writeMidi "vesicularia\\line2.mid" line2
>     writeMidi "vesicularia\\line3.mid" line3
>     writeMidi "vesicularia\\line4.mid" line4
>     writeMidi "vesicularia\\line5.mid" line5
>     writeMidi "vesicularia\\line6.mid" line6
>     writeMidi "vesicularia\\line7.mid" line7
>     writeMidi "vesicularia\\line8.mid" line8
>     writeMidi "vesicularia\\part1.mid" part1
>     writeMidi "vesicularia\\part1r.mid" part1r
>     writeMidi "vesicularia\\part2.mid" part2
>     writeMidi "vesicularia\\t1arp1.mid" t1arp1
>     writeMidi "vesicularia\\t1arp2.mid" t1arp2
>     writeMidi "vesicularia\\t1chordsM.mid" t1chordsM
>     writeMidi "vesicularia\\t1chordsFG.mid" t1chordsFG
>     writeMidi "vesicularia\\t1chordsFG2.mid" t1chordsFG2
>     writeMidi "vesicularia\\t1Bass.mid" t1Bass
>     writeMidi "vesicularia\\vesicularia.mid" vesicularia


===========================================

This version divides things in two, which prints nicely on a score but isn't as interesting.
This rule set was only used to make figures for examples of a motion-based grammar, since 
the tripple-based version created values that were difficult to visualize on a traditional 
score.

> moRulesX :: [Rule Motion (Int,Dur)]
> moRulesX = [
>     (Up, 0.2) :-> \(a,t) -> [NT (Up, (a,t))], -- ID rule
>     (Up, 0.2) :-> \(a,t) -> [NT (Up, (abs'(a-1), t/2)), NT (Up, (a+1, t/2))],
>     (Up, 0.2) :-> \(a,t) -> [NT (Up, (abs'(a-1), t/2)), NT (Down, (a+1, t/2))],
>     (Up, 0.2) :-> \(a,t) -> [NT (Up, (a+1, t/2)), NT (Up, (abs'(a-1), t/2))],
>     (Up, 0.2) :-> \(a,t) -> [NT (Up, (a+1, t/2)), NT (Down, (abs'(a-1), t/2))],
>     (Down, 0.2) :-> \(a,t) -> [NT (Down, (a,t))], -- ID rule
>     (Down, 0.2) :-> \(a,t) -> [NT (Down, (abs'(a-1), t/2)), NT (Down, (a+1, t/2))],
>     (Down, 0.2) :-> \(a,t) -> [NT (Down, (abs'(a-1), t/2)), NT (Up, (a+1, t/2))],
>     (Down, 0.2) :-> \(a,t) -> [NT (Down, (a+1, t/2)), NT (Down, (abs'(a-1), t/2))],
>     (Down, 0.2) :-> \(a,t) -> [NT (Down, (a+1, t/2)), NT (Up, (abs'(a-1), t/2))]
>     ]

> line1X = mkUpDownMusicX 8 40 lineS 1404 234 5
> mkUpDownMusicX :: Dur -> AbsPitch -> ScaleInfo -> Int -> Int -> Int -> 
>                   ([(Motion, (Int, Dur))], Music Pitch)
> mkUpDownMusicX sDur rPitch (base, scale) seed1 seed2 iters = 
>     let x = moGen2X seed1 iters sDur 
>     in  (fst x, etrans rPitch $ (uncurry toMusic) $ unzip $ applyFst (toScale base scale) $ snd x)
> moGen2X :: Int -> Int -> Dur -> ([(Motion, (Int, Dur))], [(AbsPitch,Dur)])
> moGen2X s i d = 
>     let pds = toPairs $ snd $ gen moRulesX (mkStdGen s, [NT (Up,(1,d))]) !! i
>         f (a,(b,d)) = ((a,b),d)
>     in  (pds, applyFst (moInterp 0) $ map f pds)

> line1XM = writeMidi "mo_halves.mid" $ snd line1X

=========================

Utility function to correct for zero duratio notes as a result of 
Euterpea's takeM/dropM functions:

> filterZeros :: Music a -> Music a
> filterZeros (Prim (Note d x)) = if d<=0 then rest 0 else note d x
> filterZeros (Prim (Rest d)) = rest d
> filterZeros (m1 :+: m2) = filterZeros m1 :+: filterZeros m2
> filterZeros (m1 :=: m2) = filterZeros m1 :=: filterZeros m2
> filterZeros (Modify c m) = Modify c $ filterZeros m

Other utility functions:

> abs' i = if i<0 then (-i) else 
>          if i==0 then i+1 else i

> applyFst :: ([a] -> [c]) -> [(a,b)] -> [(c,b)]
> applyFst f x = let (a,b) = unzip x in zip (f a) b

