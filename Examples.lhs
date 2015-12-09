> module Examples where
> import ChordSpaces
> import PostProc
> import ClassicalFG
> import PTGG
> import MusicGrammars
> import EuterpeaSpecial
> import System.Random
> import Euterpea.ExperimentalPlay


Let's start by creating a very simple musical grammar. Kulitta comes with a few built-in, but it is possible to define new ones. We'll use some of the datatypes defined in MusicGrammars.lhs for Roman numeral symbols. 

data CType = I | II | III | IV | V | VI | VII
    deriving (Eq, Show, Ord, Enum, Read)


We'll encode just a few simple rules and give them probabilities by hand, assuming that duration is divided in half for rules with two symbols on the right:

1. (0.3) I -> V I
2. (0.6) I -> I I
3. (0.1) I -> I
4. (0.5) V -> IV V
5. (0.4) V -> V V
6. (0.1) V -> V
7. (0.8) IV -> IV IV
8. (0.2) IV -> IV

We'll also use the MP type ("music parameter") from MusicGrammars.lhs to store the duration for each symbol. We can now write these rules in Haskell as follows. 

> r1, r2, r3, r4, r5, r6, r7, r8 :: Rule CType MP
> r1 = (I, 0.3) :-> \p -> [v (h p), i (h p)]
> r2 = (I, 0.6) :-> \p -> [i (h p), i (h p)]
> r3 = (I, 0.1) :-> \p -> [i p] 
> r4 = (V, 0.5) :-> \p -> [iv (h p), v (h p)]
> r5 = (V, 0.4) :-> \p -> [v (h p), v (h p)]
> r6 = (V, 0.1) :-> \p -> [v p] 
> r7 = (IV, 0.8) :-> \p -> [iv (h p), iv (h p)]
> r8 = (IV, 0.2) :-> \p -> [iv p] 


The argument "p" to the anonymous function ("\p -> ...", read as "given some value, p, do ... to it") in the code below refers to the parameter associated with the rules. In MusicGrammars.lhs, the function "h" divides the duration of the symbol in half. The file also provides lower-case versions of the Roman numerals that are functions to create appropriately-typed data structures for the grammar.

Now, one problem with using the rules as-is above is that they will exhibit L-System like behavior of unbalanced durations. Kulitta fixes this by allowing conditional rules, such as:

I -> if not enough duration then do nothing, otherwise (normal righthand side)

This results in a more even distribution of durations, since they cannot become infinitely small as generation progresses. We can convert all the rules to this format as follows.

> rules :: [Rule CType MP]
> rules = map (toRelDur2 (<qn)) [r1, r2, r3, r4, r5, r6, r7, r8]

Now we can generate some music with the grammar. First, we will create a start symbol and a random generator to work with. The start symbol will be a 4-measure long I-chord (4 times a wholenote, wn) in C-major (written below as "Major" with root pitch class 0, or C).


> startSym = [i (MP (4*wn) Major 0)]
> g1 = mkStdGen 42

The "gen" function creates an infinite list of sequential generative iterations. We will call it on the start symbol with a random number seed and then take the 5th generative iteration. This step returns a new random generator, g2, in addition to the abstract structure of the music.

> (g2, absStruct) = gen rules (g1, startSym) !! 5

Now we can map pitches to these chords with a classical chord space. We will impose no additional search constraints and just use Kulitta's defaults. This step also returns a new generator, g3, that we can use for the final step.

> (g3, chords) = classicalCS2 g2 (toAbsChords absStruct) [] 

And finally, we put a simple foreground on top.

> (g4, (justChords, finalMusic)) = classicalFG' g3 chords

Note: the foreground function used above actually returns two things in addition to the generator: a Music value of the chords and a Music value with melodies added. This allows one to see the before/after difference if desired.

Now we can hear the music!

> hearIt = playC defParams{strict=True} finalMusic


============================================

Supplemental code for generating figures used online:

> abstractChordMusic = transpose 60 $ tChordsToMusic $ toAbsChords absStruct
> justChordsMusic = tChordsToMusic chords

> writeEverything = do
>     writeMidi "examples\\absStruct.mid" abstractChordMusic
>     writeMidi "examples\\chords.mid" justChordsMusic 
>     writeMidi "examples\\finalMusic.mid" finalMusic
