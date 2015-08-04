Musical Grammars
Donya Quick
Last modified: 19-Dec-2014

> module MusicGrammars where
> import PTGG
> import System.Random
> import Data.List

==================================
TYPE SYNONYMS & CONSTANTS

> type Dur = Rational
> type AbsPitch = Int
> wn = 1 :: Dur
> hn = 1/2 :: Dur
> qn = 1/4 :: Dur
> en = 1/8 :: Dur
> sn = 1/16 :: Dur
> tn = 1/32 :: Dur

==================================
ALPHABETS FOR BASE SYMBOLS

Alphabet 1: Roman numerals for chords

> data CType = I | II | III | IV | V | VI | VII
>     deriving (Eq, Show, Ord, Enum, Read)

Alphabet 1: from Rohrmeier's paper "Towards a generative syntax of tonal harmony"

> data RTerm = Piece | P | -- piece/phrase (or P=Plagal for Kulitta's reuse of P)
>              TR | DR | SR | -- regions
>              T | D | S | TP | TCP | SP | DP | -- chord functions
>              C CType -- Roman numerals
>     deriving (Eq, Read)

Show function for alphabets with duration:

> showDur :: (Show a) => (a,MP) -> String
> showDur (a,MP d m k) = show a ++ "(" ++ show d ++ ")"


==================================
ALPHABETS FOR PARAMETERS

Many finite base symbol alphabets can use the same potentially infinite
alphabet of parameter symbols. Here we define a general "music parameter"
or MP for many tonal applications. It will store the current duration 
of a symbol, and the symbol's tonal context as a mode and scale root.

> data MP = MP {dur :: Dur, mode :: Mode, key :: Int}
>     deriving (Eq, Show) 

Modes include the seven usual derivatives of the C-major scale along with
chromatic and custom options. Note that Major=Ionian and Minor=Aeoloean.

> --          I       II       III        IV       V            VI      VII
> data Mode = Major | Dorian | Phrygian | Lydian | Mixolydian | Minor | Locrian | 
>             Chromatic | Custom [AbsPitch]
>     deriving (Eq, Show, Ord, Read)


A partial Enum instance is supplied for the modes with seven-note scales.
The enumFrom function is defined to loop around. For example:

    enumFrom Dorian ==> [Dorian, Phrygian, ..., Locrian, Major]

> allEnumModes = [Major, Dorian, Phrygian, Lydian, Mixolydian, Minor, Locrian]

> instance Enum Mode where
>     toEnum i = if i>=0 && i<=6 then allEnumModes !! i
>                else error "Only modes 0-6 are enumerable."
>     fromEnum Chromatic = error "Chromatic mode is not part of Enum instance."
>     fromEnum (Custom x) = error "Cannot enumerate a Custom mode."
>     fromEnum x = case findIndex (==x) allEnumModes of
>                      Nothing -> error ("Cannot enumerate unknown mode: "++show x)
>                      Just i -> i
>     enumFrom x = case findIndex (==x) allEnumModes of
>                      Nothing -> error ("Cannot enumerate from unknown mode: "++show x)
>                      Just i -> take 7 $ drop i (allEnumModes++allEnumModes)

A default MP value is one measure long (in 4/4) in the key of C-major.

> defMP = MP 1 Major 0

It is also useful to have tests for MP values and modifiers for them.

> isMaj = (==Major) . mode
> isMin = (==Minor) . mode

Modifiers on duration can be used to succinctly write transformations.
For example, to halve the duration of a parameter p::MP, one need only
write (h p) rather than something like p{dur=(dur p)/2}

> dFac x p = p{dur = dur p * x}
> h = dFac 0.5 -- half of the original duration
> q = dFac 0.25 -- a quarter of the original duration
> e = dFac 0.125

> toRelDur :: (Dur -> Bool) -> Rule a MP -> Rule a MP
> toRelDur f ((l,d):-> rf) = 
>     (l,d) :-> \p -> if f $ dur p then [NT (l,p)] else rf p

> toRelDur2 :: (Dur -> Bool) -> Rule a MP -> Rule a MP
> toRelDur2 f ((l,d):-> rf) = (l, d) :-> \p -> 
>     let xs = map (dur.snd) $ toPairs $ expand [] $ rf p
>     in  if or (map f xs) then [NT (l,p)] else rf p

For mode/key changes:

> --         C      D      E      F      G      A      B
> majModes = [Major, Minor, Minor, Major, Major, Minor, Minor]
> minModes = [Minor, Minor, Major, Minor, Minor, Major, Major]
> modalPats = enumFrom Major :: [Mode]

> majScale = [0,2,4,5,7,9,11]
> minScale = [0,2,3,5,7,8,10]

> getScale :: Mode -> [AbsPitch]
> getScale Major = majScale
> getScale Minor = minScale
> getScale m = error ("(getScale) Scale not defined for mode" ++ show m)

> modMajMin i p = let k0 = key p in
>      if mode p == Major then p{mode=majModes!!i, key=(k0+(majScale!!i)) `mod` 12} 
>      else p{mode=minModes!!i, key=(k0+(minScale!!i)) `mod` 12} 

Basic modulations on scale degrees for Major and Minor systems

> [m2, m3, m4, m5, m6, m7] = map modMajMin [1..6]


[TO-DO: MODAL VERSION OF MODULATIONS]


==================================

P = {piece, P} 
R = {TR, SR, DR} functional region symbols
K = {Cmaj, Cmin, ...} key symbols
F = {t, s, d, tp, sp, dp, tcp} functional term symbols
S = {I, II, ...} scale degree chord representations
O = {Cmaj, ...} surface chord symbols (e.g. I in K=Cmaj)

> allRTerms = [Piece, P, TR, DR, SR, T, D, S, TP, TCP, SP, DP, 
>              C I, C II, C III, C IV, C V, C VI, C VII]

> instance Show RTerm where
>     show Piece = "Piece"
>     show P = "P"
>     show TR = "TR"
>     show DR = "DR"
>     show SR = "SR"
>     show T = "T"
>     show D = "D"
>     show S = "S"
>     show TP = "TP"
>     show TCP = "TCP"
>     show SP = "SP"
>     show DP = "DP"
>     show (C x) = show x


> showRTerms :: [RTerm] -> String
> showRTerms [] = ""
> showRTerms [t] = show t
> showRTerms (t1:ts) = show t1 ++ " " ++ showRTerms (ts)


TSD Grammar Base

> tsdRules :: [Rule RTerm MP]
> tsdRules = [
>     (T, 0.25) :-> \p -> [NT (T, p)],
>     (T, 0.25) :-> \p -> [NT (T, h p), NT (T, h p)],
>     (T, 0.25) :-> \p -> [NT (T, h p), NT (D, h p)],
>     (T, 0.25) :-> \p -> [NT (D, h p), NT (T, h p)],
>     (D, 0.33) :-> \p -> [NT (D, p)],
>     (D, 0.33) :-> \p -> [NT (D, h p), NT (D, h p)],
>     (D, 0.34) :-> \p -> [NT (S, h p), NT (D, h p)],
>     (S, 0.5) :-> \p -> [NT (S, p)],
>     (S, 0.5) :-> \p -> [NT (S, h p), NT (S, h p)]
>     ]

==================================

Roman Numeral Grammar Base

> [i, ii, iii, iv, v, vi, vii] = map fc $ enumFrom I where
>      fc ct p = NT (ct,p)

Grammar from dissertation chapter 4, table 4.2 with optional let 
statements added.

> rRules1 :: Dur -> Bool -> [Rule CType MP]
> rRules1 minDur useLets = normalize $ map (toRelDur2 (<minDur)) ([
>     -- Rules for I --
>     (I, 0.187) :-> \p -> [(if isMaj p then ii else iv) (q p), v (q p), i (h p)],
>     (I, 0.187) :-> \p -> map ($ q p) [i, iv, v, i],
>     (I, 0.187) :-> \p -> [v (h p), i (h p)],
>     (I, 0.187) :-> \p -> map ($ q p) $ [i, if isMaj p then ii else iv, v, i],
>     (I, 0.252) :-> \p -> [i p],
>     -- Rules for II --
>     (II, 0.40) :-> \p -> if isMaj p then [ii p] else [iv p],
>     (II, 0.40) :-> \p -> if isMaj p then (if dur p > qn then [ii p] else [i (m2 p)]) else [ii p],
>     (II, 0.20) :-> \p -> map ($ h p) $ if isMaj p then [vi, ii] else [vi, iv],
>     -- Rules for III--
>     (III, 0.90) :-> \p -> [iii p],
>     (III, 0.10) :-> \p -> [i $ m3 p],
>     -- Rules for IV -- 
>     (IV, 0.90) :-> \p -> [iv p],
>     (IV, 0.10) :-> \p -> [i $ m4 p],
>     -- Rules for V --
>     (V, 0.10) :-> \p -> [v p],
>     (V, 0.15) :-> \p -> [iv (h p), v (h p)],
>     (V, 0.10) :-> \p -> [iii (h p), vi (h p)],
>     (V, 0.10) :-> \p -> map ($ q p) [i, iii, vi, v],
>     (V, 0.10) :-> \p -> map ($ q p) [v, vi, vii, v],
>     (V, 0.10) :-> \p -> [v (h p), vi (h p)],
>     (V, 0.10) :-> \p -> [iii p],
>     (V, 0.05) :-> \p -> [v (h p), v (h p)],
>     (V, 0.10) :-> \p -> [vii (h p), v (h p)],
>     (V, 0.10) :-> \p -> [i $ m5 p],
>     -- Rules for VI --
>     (VI, 0.70) :-> \p -> [vi p],
>     (VI, 0.30) :-> \p -> [i $ m6 p],
>     -- Rules for VII --
>     (VII, 0.50) :-> \p -> if dur p > qn then [vii p] else [i $ m7 p],
>     (VII, 0.50) :-> \p -> [i (h p), iii (h p)]
>     ] ++ if useLets then letRules else []) where
>     letRules = concatMap (\ct -> [letRule1 ct, letRule2 ct]) (enumFrom I)
>     letRule1 ct = (ct, 0.1) :-> \p -> [Let "x" [NT(ct, h p)] [Var "x", Var "x"]]
>     letRule2 ct = (ct, 0.1) :-> \p -> [Let "x" [NT(ct, q p)] [Var "x", v (h p), Var "x"]]


==================================

TESTING

> tsdGen s = putStrLn $ concat $ intersperse "  " $
>     map showDur $ toPairs $ snd $ gen tsdRules (mkStdGen s, [NT (T,MP 4 Major 0)]) !! 4

> rGen s = putStrLn $ concat $ intersperse "  " $ map showDur $ toPairs $ snd $ 
>     gen (rRules1 qn False) (mkStdGen s, [NT (I,MP 4 Major 0)]) !! 5

