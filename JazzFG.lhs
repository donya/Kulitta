Simple Jazz Foreground Algorithms
Donya Quick

Last Modified: 19-Dec-2014

> module JazzFG where
> import PTGG
> import MusicGrammars
> import System.Random
> import EuterpeaSpecial
> import ChordSpaces
> import PostProc
> import Data.List
> import ModeSpace
> import Control.Parallel.Strategies 
> import ClassicalFG 
> import Search
> import Constraints



First, we need to find the modes for Roman numerals interpreted
in a particular key/mode. The type JTriple is actually a synonym 
for TChord, but it is used for clarity to indicate that the pitch 
information represents a mode rather than a chord.

> majorModes = allModes
> minorModes = drop 5 allModes ++ (take 5 allModes)

> chordMode :: CType -> Key -> AbsMode
> chordMode ct (k,m) = 
>     let pModes = if m==Major then majorModes else minorModes
>         ctMode = pModes !! fromEnum ct
>         ck = pModes !! 0 !! fromEnum ct
>     in  t (k+ck) ctMode

> toJTriple :: (Key, Dur, CType) -> (Key, Dur, AbsMode)
> toJTriple (km,d,c) = (km, d, chordMode c km)

============================

> jazzChords :: StdGen -> [(Key, Dur, CType)] -> Constraints -> (StdGen, [(Key, Dur, AbsChord)])
> jazzChords g chords consts = 
>     let [gJ, gOPC, g'] = take 3 $ splitN g
>         jts = map toJTriple chords
>         ms = map (\(a,b,c) -> ([],c)) jts -- get just modes as JChords
>         qJ = modeSpace' alg1Temps -- subset of ModeSpace desired
>         chordsJ = greedyLet (const True) nearFallJ consts (map (eqClass qJ modeEq) ms) gJ -- random walk through qj
>         qOPC = makeRange' alg1Rans // opcEq  -- subset of OPC-space desired
>         es = map (convOPC qOPC bassRoot) chordsJ -- OPC equivalence classes for chords
>         chordsOPC = greedyProg' (const True) nearFall gOPC es -- random walk through OPC-space
>         chordsOPC' = zipWith newP jts chordsOPC -- tag with dur & mode
>     in  (g', chordsOPC')

============================

Algorithm 1: chords and a stochastic bass. Let instantiation only takes 
place at the level of Roman numerals. 

> --            r t f 7    r 2 t f 7	r=root, r=third, f=fifth
> alg1Temps = [[0,2,4,6], [0,1,2,4,6]] 

> --          bass       chords
> alg1Rans = (34,45) : take 4 (repeat (50,64))

> bassRoot (chrd, m) = (minimum chrd `mod` 12) == head (normO m)

> jazzFG1 :: StdGen -> [(Key, Dur, CType)] -> Constraints -> (StdGen, Music Pitch)
> jazzFG1 g chords consts = 
>     let [gJ, gR, gOPC, gB] = take 4 $ splitN g
>         jts = map toJTriple chords
>         ms = map (\(a,b,c) -> ([],c)) jts -- get just modes as JChords
>         qJ = modeSpace' alg1Temps -- subset of ModeSpace desired
>         chordsJ = greedyLet (const True) nearFallJ consts (map (eqClass qJ modeEq) ms) gJ
>                -- greedyProg qJ modeEq (const True) nearFallJ gJ ms -- random walk through qj
>         qOPC = makeRange' alg1Rans // opcEq  -- subset of OPC-space desired
>         es = map (convOPC qOPC bassRoot) chordsJ -- OPC equivalence classes for chords
>         chordsOPC = greedyProg' (const True) nearFall gOPC es -- random walk through OPC-space
>         chordsOPC' = zipWith newP jts chordsOPC -- tag with dur & mode
>         voices = toVoices chordsOPC' -- place in voice format
>         (gRet, bassLine) = stochBass gB $ head voices -- stochastic bassline
>     in  (gRet, instrument AcousticBass bassLine :=: 
>                vsToMusicI (repeat AcousticGrandPiano) (tail voices))

> splitN g = let (g1,g2) = split g in g1 : splitN g2

Direct interface to grammar monad:

> jazzFG1T :: StdGen -> Sentence CType MP -> Constraints -> (StdGen, Music Pitch)
> jazzFG1T g t consts = jazzFG1 g (toChords $ expand [] t) consts

> convOPC :: QSpace AbsChord -> Predicate JChord -> JChord -> EqClass AbsChord
> convOPC q pj (c,m) = filter (\x -> pj (x,m)) $ eqClass q opcEq c

> stochBass :: StdGen -> [TNote] -> (StdGen, Music Pitch)
> stochBass g [] = (g, rest 0)
> stochBass g ((km,d,p):t) = 
>     let (g', pat) = pickPattern g d p
>         (g'', t') = stochBass g' t
>     in  (g'', pat :+: t')

> pickPattern g d p = 
>     let (r,g') = randomR (0,length pats - 1) g
>         f d p = note d (pitch p)
>         pats = [f d p,
>                 if d>=hn then f qn p :+: f (d-qn) p else f d p,
>                 if d>=hn then f (d-en) p :+: f en p else f d p]
>     in  (g', pats !! r)


=============================

Algorithm 2: simple bossa nova

This approach interprets Roman numerals through three separate
chord spaces in order to cut down the task's combinatorics. 

> alg2TempsC = [[0,2,4,6], [1,2,4,6]] -- for chords
> alg2TempsB = [[0,4]] -- for bass
> alg2TempsL = [[0],[2],[4]] -- for lead

> alg2RansB = [(34,49), (34,49)]
> alg2RansC = take 4 $ repeat (50,64)
> alg2RansL = [(65,80)]

> bassRoot2 ([b1,b2], m) = normO [b1,b2] == normO [m !! 0, m!! 4]
> bassRoot2 _ = error "(bassRoot2) Bad arguments."

> alg2FilterC x = sorted x && pianoChord x

> jazzFG2 :: StdGen -> [(Key, Dur, CType)] -> Constraints -> (StdGen, Music (Pitch, Volume))
> jazzFG2 g chords consts = 
>     let gs@[gJC, gJB, gJL, gRC, gRB, gRL, gOPC_C, gOPC_B, gOPC_L, gL] = take 10 $ splitN g
>         jts = map toJTriple chords
>         ms = map (\(a,b,c) -> ([],c)) jts
>         qs@[qJC, qJB, qJL] = map modeSpace' [alg2TempsC, alg2TempsB, alg2TempsL] -- jazz spaces
>         [chordsJ, bassJ, leadJ] = -- random walk for chords
>             zipWith (\q gx -> greedyProg q modeEq (const True) nearFallJ gx ms) qs $
>             take 3 gs 
>         qOPC_C = filter alg2FilterC (makeRange' alg2RansC) // opcEq 
>         qOPC_B = makeRange alg2RansB // opcEq 
>         qOPC_L = makeRange' alg2RansL // opcEq 
>         esC = map (convOPC qOPC_C (const True)) chordsJ -- OPC equivalence classes for chords
>         esB = map (convOPC qOPC_B bassRoot2) bassJ
>         esL = map (convOPC qOPC_L (const True)) leadJ
>         chordsOPC = greedyLet (const True) nearFall consts esC gOPC_C -- random walk through OPC-space
>         bassOPC = greedyLet (noCPL 7) nearFall consts esB gOPC_B -- random walk for bass
>         leadOPC = greedyLet (noCPL 7) nearFall consts esL gOPC_L -- random walk for lead
>         [cc, bc, lc] = map (zipWith newP jts) [chordsOPC, bassOPC, leadOPC] -- tag with dur & mode
>         cm = bossaChords cc
>         bm = bossaBass bc
>         (gRet, lm) = bossaLead gL lc
>     in  (gRet, chord [addVolume 127 $ instrument AcousticBass bm,
>                       addVolume 75 $ instrument AcousticGrandPiano cm, 
>                       addVolume 127 $ instrument Flute lm])

> jazzFG2T :: StdGen -> Sentence CType MP -> Constraints ->  (StdGen, Music (Pitch, Volume))
> jazzFG2T g t consts = jazzFG2 g (toChords $ expand [] t) consts

> bossaBass :: [TChord] -> Music Pitch
> bossaBass [] = rest 0
> bossaBass ((km,d,c@[p1,p2]):t) = 
>     if d > wn then bossaBass ((km,wn,c):(km,d-wn,c):t) else
>     if d == wn then f1 p1 p2 :+: bossaBass t else
>     if d == hn then f2 p1 p2 :+: bossaBass t  else f3 p1 d :+: bossaBass t where
>     f1 b1 b2 = f2 b1 b2 :+: f2 b2 b1
>     f2 b1 b2 = f3 b1 (qn+en) :+: f3 b2 en 
>     f3 b1 d = note d (pitch b1)
> bossaBass _ = error "(bossaBass) Bad input"

> bossaChords :: [TChord] -> Music Pitch
> bossaChords [] = rest 0
> bossaChords ((km,d,c):t) = 
>     if d > wn then bossaChords ((km,wn,c):(km,d-wn,c):t) else 
>     if d==wn then f1 c :+: bossaChords t else f2 d c :+: bossaChords t where
>     f1 c = let c' = f2 en c in rest qn :+: c' :+: rest qn :+: c' :+: rest qn
>     f2 d c = chord $ map (\p -> note d $ pitch p) c

> bossaLead :: StdGen -> [TChord] -> (StdGen, Music Pitch)
> bossaLead g ts = 
>     let ls = take (length ts - 1) (repeat False) ++ [True]
>         v = head $ toVoices ts
>         (g', v') = addFgToVoice jConsts (foreFunsJ defConsts) g v
>     in  (g', vsToMusic [v']) where
>     foreFunsJ c = [(0.5, f1 c), (0.5, f2 c)] :: [(Double, ForeFun)]
>     jConsts = CConstants 2 3 0.3 0.5 0.8 7


