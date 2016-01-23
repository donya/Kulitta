Classical Foreground Module
Donya Quick

For doctoral thesis. Based on work from 690/691 (Master's thesis): 
http://haskell.cs.yale.edu/?post_type=publication&p=260

Last modified 19-Dec-2014


> module Kulitta.Foregrounds.ClassicalFG where
> import Kulitta.PTGG
> import Kulitta.Grammars.MusicGrammars
> import System.Random
> import Kulitta.EuterpeaSpecial
> import Kulitta.ChordSpaces hiding (i)
> import Kulitta.PostProc
> import Data.List
> import Kulitta.Search
> import Kulitta.Constraints


> data CConstants = CConstants {
>     ntLimC :: Int, -- limit for neighboring tone distance
>     ptLimC :: Int, -- limit for passing tone distance
>     pHalfC :: Double, -- probability of dividing a note's duration in half (alternative is x-en and en)
>     pTieC :: Double, -- probability of tying two identical notes
>     rootBassThreshC :: Double, -- probability of enforcing that the bass be the root
>     noCPLThreshC :: Int } -- voice-leading maximum, setting to 0 forces nearest neighbor fallback

A set of default constants that work pretty well in most cases.

> defConsts = CConstants 2 3 0.5 0.5 0.8 7

 

Get all pitches shared between the scales of two TNotes

> allPs :: TNote -> TNote -> [AbsPitch]
> allPs t1 t2 = 
>     let (o1, o2) = (tnP t1 `div` 12, tnP t2 `div` 12)
>         [oMin, oMax] = sort [o1, o2]
>         offs = map (12*) [oMin-1, oMin, oMax, oMax+1]
>         (s1, s2) = (baseScale $ tnK t1, baseScale $ tnK t2)
>     in  nub $ concatMap (\o -> t o [s | s<-s1, elem s s2]) offs where
>     baseScale :: Key -> [AbsPitch]
>     baseScale (k,m) = normOP $ t k (getScale m)

A foreground function, ForeFun, is a stochastic operation on two notes that 
may or may not add an additional pitch between them. Duration selection 
happens as a second step later since it involves altering the durations of 
the surrounding notes.

> type ForeFun = StdGen -> TNote -> TNote -> (StdGen, Maybe AbsPitch)

A passing tone is a note between two chodal tones. Here, the definition is a
litle broader than what would be assumed in standard music theory since the 
chordal tones need not be close together. As a result, the "passing tones" 
chosen by pickPT below may end up being other categories of non-chordal 
tones in music theory.

> pickPT :: AbsPitch -> ForeFun
> pickPT lim g t1 t2 = 
>     let [pMin, pMax] = sort [tnP t1, tnP t2]
>         f x = x>pMin && x<pMax && (x - pMin <=lim || pMax - x <=lim)
>         ps' = [x | x<-allPs t1 t2, f x]
>         (iNew, g') = randomR (0, length ps' - 1) g
>     in  if pMin == pMax || null ps' then (g, Nothing)
>         else (g', Just $ ps' !! iNew)

Neighboring tones are handled similarly to passing tones. Again, the 
definition here is fairly broad and may produce other categorie of non-
chordal tones as a result.

> pickNT :: AbsPitch -> ForeFun
> pickNT lim g t1 t2 = 
>     let [pMin, pMax] = sort [tnP t1, tnP t2]
>         f x =  (x < pMin && pMin - x <= lim) || (x > pMax && x - pMax <= lim)
>         ps' = [x | x<-allPs t1 t2, f x]
>         (iNew, g') = randomR (0, length ps' - 1) g
>     in  if pMin == pMax || null ps' then (g, Nothing)
>         else (g', Just $ ps' !! iNew)

Functions for adding anticipations and repetitions are simple to define. 
A "do nothing" operations is also a useful option to have.

> anticip, rept, doNothing :: ForeFun
> anticip g t1 t2 = (g, Just $ tnP t2)
> rept g t1 t2 = (g, Just $ tnP t1)
> doNothing g t1 t2 = (g, Nothing)

Some of the functions above need access to constants. We use the 
CConstants type for this, yielding a collection of functions of 
type CConstants -> ForeFun. Finally, these are each bundled with 
a probability of application for each voice.

> f1 = pickPT . ptLimC 
> f2 = pickNT . ntLimC 
> [f3, f4, f5] = map const [anticip, rept, doNothing]

> allFFs :: CConstants -> [[(Double, ForeFun)]]
> allFFs c = 
>    [[(0.3, f1 c), (0.1, f2 c), (0.6, f5 c)], -- S (sopranno)
>    [(0.3, f1 c), (0.7, f5 c)], -- A (alto)
>    [(0.1, f1 c), (0.9, f5 c)]]  -- T (tennor)
>    ++ repeat [(1.0, f5 c)] -- B (bass) and lower

> splitP :: CConstants -> StdGen -> AbsPitch -> TNote -> (StdGen, [TNote])
> splitP consts g newP t = 
>     let (r, g') = randomR (0,1.0::Double) g
>         dNew = if r < pHalfC consts then tnD t / 2 else en
>     in  (g', [(tnK t, tnD t - dNew, tnP t), (tnK t, dNew, newP)])

Here we assume that the contexts include the voice in question. The |i| argument is the voice number.
The function only returns modifications of the first chord.

> addFgToVoice :: CConstants -> [(Double, ForeFun)] -> StdGen -> [TNote] -> (StdGen, [TNote])
> addFgToVoice c foreFuns g (t1:t2:ts) =
>     let (j, g1) = randomR (0, 1.0) g
>         fFun = chooseFF j foreFuns
>         (g2, t1') = applyForeFun c g1 t1 t2 fFun
>         (g3, tRest) = addFgToVoice c foreFuns g2 (t2:ts)
>     in  (g3, t1' ++ tRest) where
>     chooseFF j [x] = snd x
>     chooseFF j ((p,x):t) = if j<p then x else chooseFF (j-p) t
>     chooseFF j [] = error "(chooseFF) Nothing to choose from!"
>     applyForeFun c g t1 t2 fFun = 
>         let (g1, newP) = fFun g t1 t2
>         in  case newP of Nothing -> (g1, [t1])
>                          Just x -> splitP c g1 x t1
> addFgToVoice c foreFuns g x = (g, x)

After adding foreground elements, ties can be considered. The following |stochTie| 
function stochastically ties notes in a voice. 

> stochTie :: CConstants -> StdGen -> [TNote] -> (StdGen, [TNote])
> stochTie consts g (t1:t2:ts) = 
>     let (r, g1) = randomR (0, 1.0::Double) g
>         (g2, (t2':ts')) = stochTie consts g1 (t2:ts)
>         (d1,d2') = (tnD t1, tnD t2')
>     in  if tnP t1 == tnP t2' && r < pTieC consts 
>         then (g2, (tnK t1, d1+d2', tnP t1):ts')
>         else (g2, t1 : t2' : ts')
> stochTie consts g ts = (g, ts)

Finally, the |addFG| function puts all of these elements together.

> addFG :: CConstants -> StdGen -> [[TNote]] -> (StdGen, [[TNote]])
> addFG c g vs = let (g', vs') = fgRec c g 0 vs in tieRec c g' vs' where
>     fgRec c g i vs = if i >= length vs || i<0 then (g, vs) else 
>         let (g', v') = addFgToVoice c (allFFs c !! i) g (vs !! i)  
>             vs' = take i vs ++ [v'] ++ drop (i+1) vs
>         in  fgRec c g' (i+1) vs'
>     tieRec c g [] = (g, [])
>     tieRec c g (v:vs) = 
>         let (g1,v') = stochTie c g v
>             (g2,vs') = tieRec c g1 vs
>         in  (g2, v':vs')

=============================================

There are two steps to adding a classical foreground: 
1. Traversing an appropriate chord space.
2. Adding foreground elements.

These steps are separated and presented with different type interfaces.
From a Term CType, a classical foreground can be added by using just the
classicalFG function.

> classicalFG :: StdGen -> Sentence CType MP -> (StdGen, (Music Pitch, Music Pitch))
> classicalFG g t = 
>    let consts = sort $ findInds [] t
>        rChords = toChords (expand [] t) 
>    in  classicalFGR g rChords consts

However, there are some instances where more control is desirable, such as
if we are working with Let statments or perhaps want to supply a progression 
manually rather than using Term. The following functions allow adding a 
foreground to different intermediate types.

> classicalFGR :: StdGen -> [RChord] -> Constraints -> (StdGen, (Music Pitch, Music Pitch))
> classicalFGR g rcs consts = 
>    let (g1, csChords) = classicalCS g rcs consts
>    in  classicalFG' g1 csChords

> classicalFG' :: StdGen -> [TChord] -> (StdGen, (Music Pitch, Music Pitch))
> classicalFG' g aChords' = 
>    let (g4,csFG) = addFG defConsts g $ reverse $ toVoices aChords'
>        is = [Bassoon, EnglishHorn, Clarinet, Oboe, SopranoSax] 
>        fgM = vsToMusicI is $ reverse csFG
>        csM = vsToMusicI is $ toVoices aChords'
>    in  (g4, (csM, fgM))

Similarly, there are instances when we may want to use a classical chord space, but
not add a classical foreground. This can be useful for mixing styles.

> classicalCS :: StdGen -> [RChord] -> Constraints -> (StdGen, [TChord])
> classicalCS g rcs consts = 
>     classicalCS2 g (map toAbsChord rcs) consts 

> classicalCS2 :: StdGen -> [TChord] -> Constraints -> (StdGen, [TChord])
> classicalCS2 g aChords consts = 
>    let justChords = map (\(a,b,c) -> c) aChords
>        (g1,g2) = split g
>        (g3, eqs) = classBass 0.8 g2 $ map (eqClass satbOP opcEq) justChords
>        csChords = greedyLet (noCPL 7) nearFall consts eqs g3 
>        aChords' = zipWith (\(a,b,c) d -> (a,b,d)) aChords csChords
>    in  (g3, aChords')

> classicalCS2' :: Predicate AbsChord -> [(AbsPitch, AbsPitch)] -> StdGen -> [TChord] -> Constraints -> (StdGen, [TChord])
> classicalCS2' fFilter ranges g aChords consts = 
>    let justChords = map (\(a,b,c) -> c) aChords
>        (g1,g2) = split g
>        satbOPx = filter fFilter (makeRange ranges) // opcEq
>        (g3, eqs) = classBass 0.8 g2 $ map (eqClass satbOPx opcEq) justChords
>        csChords = greedyLet (noCPL 7) nearFall consts eqs g3 
>        aChords' = zipWith (\(a,b,c) d -> (a,b,d)) aChords csChords
>    in  (g3, aChords')


The classicalCS2 function uses a stochastic filter over equivalence classes.
This filter enforces that the bass holds the root with a certain probability 
(the "thresh" value). If the constraints can't be met, the bass is allowed 
to deviate from this rule for the sake of producing a result.

> classBass :: Double -> StdGen -> [EqClass AbsChord] -> (StdGen, [EqClass AbsChord])
> classBass thresh g [] = (g, [])
> classBass thresh g (e:es) = 
>     let (r,g') = randomR (0,1.0::Double) g
>         e' = if r > thresh then e else filter rootFilter e
>         e'' = if null e' then e else e'
>         (g'', es') = classBass thresh g es
>     in  (g'', e'':es') where
>     rootFilter :: Predicate AbsChord
>     rootFilter x = or $ map (opcEq x) [[0,0,4,7], [0,0,3,7], [0,0,3,6]]


The code so far has only made use of the greedy approaches to constraint 
satisfaction. As an alternative, the following version handles constraint 
satisfaction differently. Two MIDI files are produced, one without melodic 
elements and one with them. There are definite pros and cons to this 
constraint satisfaction approach.
	Pros: all constraints will be 100% satisfied if a solution is found.
	Cons: existence of a solution is not guaranteed and the runtime will be
	      quite long if solutions are sparse.

> classicalFG2 :: StdGen -> Sentence CType MP -> FilePath -> FilePath -> IO ()
> classicalFG2 g t fn1 fn2 = do
>    let aChords = toAbsChords (expand [] t)
>        justChords = map (\(a,b,c) -> c) aChords
>        (g1,g2) = split g
>        qSpace = satbOP' g1
>        ecs = map (eqClass qSpace opcEq) justChords
>        cons = findInds [] t
>    (x, csChords) <- findSoln2 cons (progL 10) ecs 
>    let aChords' = zipWith (\(a,b,c) d -> (a,b,d)) aChords csChords
>        (g4,csFG) = addFG defConsts g2 $ reverse $ toVoices aChords'
>        is = [Bassoon, EnglishHorn, Clarinet, Oboe] 
>        fgM = vsToMusicI is $ reverse csFG
>        csM = vsToMusicI is $ toVoices aChords'
>    writeMidi fn1 fgM 
>    writeMidi fn2 csM
