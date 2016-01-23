Post Processing Module to Link Grammar with OPTIC Functions
Donya Quick and Paul Hudak
Last modified: 19-Dec-2014
For paper: Grammar-Based Automated Music Composition in Haskell

Post processing module to turn Terms into music using Euterpea.

> module Kulitta.PostProc where
> import Kulitta.EuterpeaSpecial 
> import Kulitta.PTGG 
> import Kulitta.Grammars.MusicGrammars
> import Kulitta.ChordSpaces
> import Data.List
> import System.Random

Intermediate types:
(NOTE: AbsPitch = PitchNum)

> type Key = (AbsPitch, Mode)
> type RChord = (Key, Dur, CType)
> type TChord = (Key, Dur, AbsChord)
> type TNote = (Key, Dur, AbsPitch)
> type Voice = [TNote]

Accessing the members of a TNote:

> tnK (k,d,p) = k
> tnD (k,d,p) = d
> tnP (k,d,p) = p
> newP (k,d,p) p' = (k,d,p')


The goal using these intermediate types is the following:

INPUT	     STEP            OUTPUT         FUNCTION
Seeds -----(grammar)-------> Sentence       gen
Sentence --(mode info)-----> [TChord]       toAbsChords
[TChord] ------------------> [Voice] 		toVoices
[Voice] -------------------> Music Pitch    vsToMusic or vsToMusicI


> unTerm :: [Term a MP] -> [(Key, Dur, a)]
> unTerm = map (\(a,mp) -> ((key mp, mode mp), dur mp, a)) . toPairs . expand []

> toChords :: [Term CType MP] -> [RChord]
> toChords = unTerm

> toAbsChords :: [Term CType MP] -> [TChord]
> toAbsChords ts = map toAbsChord $ toChords ts

> toAbsChord :: RChord -> TChord
> toAbsChord ((k,m),d,c) = ((k,m), d, t k $ toAs c m)

We also provide an alternate version that doesn't use diminished chords.

> toAbsChordNoDim :: RChord -> TChord
> toAbsChordNoDim ((k,m),d,c) = ((k,m), d, t k $ toAsNoDim c m)

> toAbsChordsNoDim :: [Term CType MP] -> [TChord]
> toAbsChordsNoDim ts = map toAbsChordNoDim $ toChords ts

Conversion of a single chord to a mode rooted at zero:

> toAs :: CType -> Mode -> [AbsPitch]
> toAs ct m = 
>     let s = getScale m ++ map (+12) s -- fininite scale
>         i = head $ findIndices (==ct) [I, II, III, IV, V, VI, VII] -- can be updated w/enum
>     in  map (s !!) $ map (+i) [0,2,4]

> toAsNoDim :: CType -> Mode -> [AbsPitch]
> toAsNoDim ct m = 
>     let s = getScale m ++ map (+12) s -- fininite scale
>         i = head $ findIndices (==ct) [I, II, III, IV, V, VI, VII] -- can be updated w/enum
>     in  map (s !!) $ fixDim $ map (+i) [0,2,4] where
>         fixDim x = if optEq x [0,3,6] then t (head x) [0,3,7] else x



Transposition using a key (to avoid C-major assignment only):

> atTrans :: AbsPitch -> [(Key, Dur, AbsChord)] -> [(Key, Dur, AbsChord)]
> atTrans a = map (\((k,m),d,c) -> (((k+a) `mod` 12,m), d, t a c)) 

map (\((k,m),d,c) -> ((fixK k a m,m),d, t (a `mod` 12) c)) 

The toCords functon does a similar thing, but returns a CType and 
its key/mode context without performing the conversion to AbsChord.

> ctTrans :: AbsPitch -> [(Key, Dur, CType)] -> [(Key, Dur, CType)]
> ctTrans a = map (\((k,m),d,c) -> (((k+a) `mod` 12,m),d,c)) 

map (\((k,m),d,c) -> ((fixK k a m,m),d,c)) 

> fixK k a Major = (k + a) `mod` 12
> fixK k a Minor = ((k + a) `mod` 12) + 12


Conversion of intermediate type to Music Pitch:

> tChordsToMusic :: [TChord] -> Music Pitch
> tChordsToMusic = line . map f  where
>     f ((k,m),d, as) = chord $ map (\a -> note d (pitch a)) as


============ SPLITTING VOICES APART ===========

The code here places TChords into a form more suitable
for additional musical processing. A Voice is a list of
pitches with duration and key/mode context. 

> toVoices :: [TChord] -> [Voice]
> toVoices ts = 
>     let (ks,ds,ps) = unzip3 ts
>     in  map (\v -> zip3 ks ds v) $ Data.List.transpose ps

> toNotes :: Voice -> Music Pitch
> toNotes = line . map (\(k,d,p) -> note' d p) where
>     note' d p = if p<0 then rest d else note d (pitch p)

> vsToMusic :: [Voice] -> Music Pitch
> vsToMusic = 
>     chord . map toNotes 

> vsToMusicI :: [InstrumentName] -> [Voice] -> Music Pitch
> vsToMusicI is = 
>     chord . zipWith (\i m -> instrument i m) is . map toNotes







