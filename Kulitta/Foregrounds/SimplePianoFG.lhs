> module Kulitta.Foregrounds.SimplePianoFG(
>     simplePianoFG1,
>     simplePianoFGMel,
>     simplePianoFGArp
>     ) where
> import Kulitta

> import Data.List
> import System.Random
> import Kulitta.EuterpeaSpecial
> import Kulitta.Grammars.MusicGrammars
> import Kulitta.Foregrounds.ClassicalFG
> import Kulitta.ChordSpaces

First, a chorale-like style, but playable on piano.

> smallRange = [absPitch (G,2)..absPitch (G,5)]



> smallSpace = makeRange2' $ take 4 $ repeat smallRange

> okSpacing1 [l1,l2,r1,r2] = l2 - l1 <= 12 && r2-r1 <= 12

> makeRange2' :: [[PitchNum]] -> [AbsChord]
> makeRange2' = foldr (\lu xs -> [(a:b) | a<-lu, b<-xs, psort (a:b)]) [[]] where
>     psort (a:b:t) = a<b
>     psort _ = True 

> isDim = optcEq [0,3,6] -- for locating diminished triads

> lhTemps = [[0,2,4], [0,4]]
> rhTemps = [[0,2,4], [0,3], [0,5]]

classicalCS2 :: StdGen -> [TChord] -> Constraints -> (StdGen, [TChord])
classicalCS2' fFilter ranges g aChords consts

> simplePianoFG1 :: Sentence CType MP -> StdGen -> (Music Pitch, Music Pitch)
> simplePianoFG1 terms g0 = 
>     let k = findInds [] terms -- determine what Let constraints exist
>         triads = toAbsChords terms
>         lr = (43, 60) -- where the left hand can play
>         rr = (60, 79) -- where the right hand can play
>         rans = [lr, lr, rr, rr] -- ranges for each of 4 voices
>         pianoFilter [a,b,c,d] = b-a <= 12 && d - c <= 8
>         (g1, newChords) = classicalCS2x pianoFilter rans g0 triads k
>         (g2, (lhM, rhM)) = classicalFGx g1 newChords
>     in  (lhM, rhM)

Modification of the classical foreground code to address piano playability.

> classicalCS2x :: Predicate AbsChord -> [(AbsPitch, AbsPitch)] -> StdGen -> [TChord] -> Constraints -> (StdGen, [TChord])
> classicalCS2x fFilter ranges g aChords consts = 
>    let justChords = map (\(a,b,c) -> c) aChords
>        (g1,g2) = split g
>        satbOPx = filter fFilter (makeRange' ranges) // opcEq
>        (g3, eqs) = classBass 1.0 g2 $ map (eqClass satbOPx opcEq) justChords
>        csChords = greedyLet (noCPL 7) nearFall consts eqs g3 
>        aChords' = zipWith (\(a,b,c) d -> (a,b,d)) aChords csChords
>    in  (g3, aChords')

> classicalFGx :: StdGen -> [TChord] -> (StdGen, (Music Pitch, Music Pitch))
> classicalFGx g aChords' = 
>    let vs = toVoices aChords'
>        (g1,csFG) = addFG defConsts{rootBassThreshC=1.0} g [vs !! 3]
>        (g2,csFG2) = addFG defConsts{rootBassThreshC=1.0} g1 [vs !! 0]
>        rh = csFG ++ [vs !! 2]
>        lh = [vs !! 1] ++ csFG2
>    in  if length vs == 4 then (g2, (vsToMusic lh, vsToMusic rh))
>        else error "classicalFGx only works on chords with 4 voices"

> tieLast2 xs@(v1:v2:v3) = 
>     let ((k,d,a):(k',d',a'):t) = reverse xs
>     in  if a==a' then reverse ((k,d+d',a):t) else xs
> tieLast2 xs = xs


===================================

Scale-based piano pieces with a simple right and left hand.

> simplePianoFGMel :: Sentence CType MP -> StdGen -> (StdGen, (Music Pitch, Music Pitch))
> simplePianoFGMel terms g0 = 
>     let aChords = toAbsChords terms -- convert to basic triads
>         k = findInds [] terms -- find let constraints
>         (g1, lhPCs) = simpleLH g0 aChords -- simplify lefthand pitch classes
>         lhSpace = filter (\[a,b] -> b-a <= 12 && a<=b) (makeRange [(40,59),(40,59)]) // opcEq
>         eqsL = map (eqClass lhSpace opcEq) lhPCs -- locate lefthand equivalence classes
>         (g2,g3) = split g1 -- get new generators
>         lhChords = greedyLet (const True) defFall k  eqsL g2 -- lefthand chords in OPC space
>         lhTChords = zipWith (\(k,d,_) x -> (k,d,x)) aChords lhChords -- reattach durations
>         (g4, lh) = lhFG g3 lhTChords -- create lefthand foreground
>         ----
>         (g5, rhPCs) = simpleRH g4 aChords -- simplify righthand pitch classes
>         rhSpace = (filter (\xs -> maximum xs - minimum xs <= 9) (makeRange [(60,80),(60,80)]) // opcEq) ++
>                   (map (\x -> [[x]]) [60..80])
>         eqsR = map (eqClass rhSpace opEq) rhPCs -- locate righthand equivalence classes
>         (g6,g7) = split g5 -- split generator again
>         rhChords = greedyLet (smoothMel 4) nearestMel k eqsR g6 -- pick righthand chords in OPC space
>         rhTChords = zipWith (\(k,d,_) x -> (k,d,x)) aChords rhChords -- reattach durations
>         (g8, rh) = rhFG g7 rhTChords -- create scale patterns with passing/neighboring tones
>     in  (g8, (lh, rh)) -- return each hand's part as a separate music value

> simpleLH :: StdGen -> [TChord] -> (StdGen, [AbsChord])
> simpleLH g [] = (g, [])
> simpleLH g [(k,d,[r,t,f])] = 
>     let (g', x) = choose g [[r,f]] -- , [r,f]]
>     in  (g', [x])
> simpleLH g ((k,d,[r,t,f]):xs) = 
>     let (g', x') = choose g [[r,t], [r,f]] -- [r,t], [r,f]]
>         (g'', xs') = simpleLH g' xs
>     in  (g'', x':xs')
> simpleLH g _ = error "simpleLH only works on 3 note combinations"

> simpleRH :: StdGen -> [TChord] -> (StdGen, [AbsChord])
> simpleRH g [] = (g, [])
> simpleRH g [(k,d,[r,t,f])] = -- last one, pick either a root or third
>     let (g', x') = choose g [[r], [t]]
>     in  (g', [x'])
> simpleRH g ((k,d,[r,t,f]):xs) = 
>     let (g', x') = choose g [[r,t], [t,f]]
>         (g'', xs') = simpleRH g' xs
>     in  (g'', x':xs')
> simpleRH g _ = error "simpleRH only works on 3 note combinations"

> lhFG :: StdGen -> [TChord] -> (StdGen, Music Pitch)
> lhFG g [] = (g, rest 0)
> lhFG g [(k,d,c)] = (g, chord $ map (note d . pitch) c)
> lhFG g ((k,d,c):t) = 
>     let (g', t') = lhFG g t
>         (g'', x) = pickPat d c g'
>     in  (g'', x :+: t') where
>     pickPat d [a1,a2] g = 
>         let [p1,p2] = map pitch [a1,a2]
>             (g', x) = choose g [--note d p1 :=: note d p2] --,
>                                 note (d/2) p1 :+: note (d/2) p2,
>                                 note (d/2) p2 :+: note (d/2) p1] 
>         in  (g', x)
>     pickPat d x g = error "lhFG only works on 2 note combinations"


> rhFG :: StdGen -> [TChord] -> (StdGen, Music Pitch)
> rhFG g chords = 
>     let v = makeVoice chords
>         (g', v') = makeMel g v 
>         --(g', vs) = addFG defConsts g [v] -- REGULAR CHORALE FG
>         -- =================== TO DO: PUT PASSING TONE STUFF HERE ==================
>     in  (g', vsToMusic [v']) where
>     makeVoice [] = []
>     makeVoice ((k,d,[x1]):t) = (k,d,x1) : makeVoice t
>     makeVoice ((k,d,[x1,x2]):t) = (k,d/2,x1) : (k,d/2,x2) : makeVoice t
>     makeVoice (x:t) = error ("Unsupported structure: "++show x)

Given a voice, add passing and neighboring tones.

> makeMel :: StdGen -> Voice -> (StdGen, Voice)
> makeMel g [] = (g, [])
> makeMel g [x] = (g, [x])
> makeMel g (x1@((k1,m1),d1,p1):x2@((k2,m2),d2,p2):xs) = 
>     let (g',pt) = pickPT 4 g x1 x2 -- next stochastic choice
>         (g'', rest) = makeMel g' (x2:xs) --- rest of melody
>     in  case pt of Nothing -> makeMelNT g'' (x1 : rest) -- passing tone not possible
>                    Just v -> (g'', ((k1,m1),d1/2,p1) : ((k1,m1),d1/2,v) : rest) 
>     where
>     makeMelNT :: StdGen -> Voice -> (StdGen, Voice)
>     makeMelNT g [] = (g, [])
>     makeMelNT g [x] = (g, [x])
>     makeMelNT g (x1@((k1,m1),d1,p1):x2@((k2,m2),d2,p2):xs) = 
>         let (g',pt) = pickNT 4 g x1 x2 -- next stochastic choice
>         in  case pt of Nothing -> (g', (x1 : x2: xs)) -- passing tone not possible
>                        Just v -> (g', ((k1,m1),d1/2,p1) : ((k1,m1),d1/2,v) : x2 :xs)


Smoth melody constraint and earest-neighbor fallback for melody construction.

> smoothMel :: AbsPitch -> Predicate (AbsChord, AbsChord)
> smoothMel thresh ([], c2) = error "Empty chord"
> smoothMel thresh (c1, []) = error "Empty chord"
> smoothMel thresh (c1, c2) = abs(last c1-head c2) < thresh

> nearestMel :: Fallback AbsChord
> nearestMel [] g c = error ("Empty equivalence class for: "++show c)
> nearestMel e g c = 
>     let p = last c -- this is the current pitch
>         ps = map head e -- these are all possible next pitches
>         dists = map (\x -> abs(p-x)) ps -- all distances
>         minDist = minimum dists 
>         (g', i) = choose g (findIndices (\x -> x == minimum dists) dists)
>     in  (g', e !! i)

==============================================

Arpeggio-based pieces, playable on piano with pedal. They may not 
always be best played with the indicated split of right and left hands.
Sometimes the lowest note of the right-hand part may be better played
by the left-hand, although it can difficult to automatically represent 
on a score this way with software such as MuseScore. 

> simplePianoFGArp :: Sentence CType MP -> StdGen -> (StdGen, (Music Pitch, Music Pitch))
> simplePianoFGArp terms g0 = 
>     let aChords = toAbsChords terms
>         k = findInds [] terms
>         (g1, tcs) = classicalCS2x' g0 aChords k
>         (tLH, tRH) = splitTChords 1 tcs
>         rhM = toArpMusic tRH
>         lhM = vsToMusic $ toVoices tLH
>     in  (g1, (lhM, rhM))

Another redoing of the chorale-inspired chord spaces.

> classicalCS2x' :: StdGen -> [TChord] -> Constraints -> (StdGen, [TChord])
> classicalCS2x' g aChords consts = 
>    let justChords = map (\(a,b,c) -> c) aChords
>        (g1,g2) = split g
>        (g3, eqs) = classBass2 0.8 g2 $ map (eqClass satbOPx opcEq) justChords
>        csChords = greedyLet myClass nearFall consts eqs g3 
>        aChords' = zipWith (\(a,b,c) d -> (a,b,d)) aChords csChords
>    in  (g3, aChords') where
>    satbOPx = satbChordsx // opEq 
>    satbChordsx = filter (\x -> arpFilter x && satbFilter x) (makeRange satbRangesx)
>    satbRangesx = [(30,60), (47,67), (52,76), (60,81)]

> classBass2 :: Double -> StdGen -> [EqClass AbsChord] -> (StdGen, [EqClass AbsChord])
> classBass2 thresh g [] = (g, [])
> classBass2 thresh g [e] = classBass 1.0 g [e]
> classBass2 thresh g (e:es) = 
>     let (g',e') = classBass thresh g [e] 
>         (g'', es') = classBass2 thresh g' es
>     in  (g'', e' ++ es')

Constraint for appropriate spacings between voices (treated as 4-note chords,
not yet arpeggiated).

> arpFilter :: Predicate AbsChord
> arpFilter = arpFilterSub . tail where
>     arpFilterSub (p1:p2:ps) = p1-p2 <= 6 && arpFilterSub (p2:ps)
>     arpFilterSub x = True

> myClass (c1,c2) = c1 /= c2 && noCPL 7 (c1,c2)

For splitting a TChord into lefthand and righthand sections by voice
count. For example, if amt = 1, a single voice will end up in the 
left hand portion.

> splitTChords :: Int -> [TChord] -> ([TChord], [TChord])
> splitTChords amt chords = unzip $ map (f amt) chords where
>     f amt (km, d, x) = ((km, d, take amt x), (km, d, drop amt x))

Arpeggiate a bunch of TChords.

> toArpMusic :: [TChord] -> Music Pitch
> toArpMusic [] = rest 0
> toArpMusic [(k,d,aps)] = chord $ map (note d . pitch) aps
> toArpMusic (x:t) = (rest sn :+: vsToMusic [toArp x]) :+: toArpMusic t

> toArp :: TChord -> [TNote]
> toArp (km@(k,m), d, aps) = 
>     let scale = t k (if m==Major then majScale else minScale)
>         arpNotes = takeDur (d-sn) $ concat $ repeat $ map (\x -> (km,sn,x)) aps
>     in  if d <= sn then [(km, d, head aps)]
>         else arpNotes where
>     takeDur :: Dur -> [TNote] -> [TNote]
>     takeDur d [] = []
>     takeDur 0 xs = []
>     takeDur d (h@(k, d', x):t) =
>         if d >= d' then h : takeDur (d-d') t else [(k, d, x)]

