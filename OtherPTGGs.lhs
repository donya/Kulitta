PTGG examples with non-musical alphabets
Donya Quick
Last modified: 03-Sept-2015

> module OtherPTGGs where
> import PTGG
> import System.Random
> import Data.List
> import Prelude hiding (Word)

A simple example using parameters in a PTGG to control selection 
of pronouns in English sentences (see values exF and exM).

> data WordNT = S | NP | VP | Adj | W Word
>     deriving (Eq, Show)
> data Word = BOB | ALICE | DRIVES | HIS | HER | CAR
>     deriving (Eq, Show)
> data WordP = M | F | N
>     deriving (Eq, Show)

> r1 = (S, 0.5)   :-> \p -> [NT (W BOB, M), NT (VP, M)] 
> r2 = (S, 0.5)   :-> \p -> [NT (W ALICE, F), NT (VP, F)]
> r3 = (VP, 1.0)  :-> \p -> [NT (W DRIVES, N), NT (NP, p)]
> r4 = (NP, 1.0)  :-> \p -> [NT (Adj, p), NT (W CAR, N)]
> r5 = (Adj, 1.0) :-> \p -> if p==M then [NT (W HIS, p)] else [NT (W HER, p)]

> rs = [r1,r2,r3,r4,r5]

> showIt (NT (W w, p)) = show w
> showIt (NT (x,p)) = show x
> showIt _ = error "Not supported"

> tgen s = putStrLn $ concat $ intersperse " " $ map showIt $ snd $ 
>     gen rs (mkStdGen s, [NT (S,N)]) !! 10

> exF = tgen 0
> exM = tgen 1

=================================

The following example produces the sequence A^n B^n C^n, which is 
impossible for usual CFGs to produce. The grammar is deterministic 
so the random number seed supplied to gen can be constant.

> data ABC = St | A | B | C 
>     deriving (Eq, Show, Ord)

> abcRules1 = [
>     (St, 1.0) :-> \n -> if n<=0 then [] else [NT (A, n), NT (B, n), NT (C, n)],
>     (A, 1.0) :-> \n -> if n>1 then [NT (A, 0), NT (A, n-1)] else [NT (A, 0)],
>     (B, 1.0) :-> \n -> if n>1 then [NT (B, 0), NT (B, n-1)] else [NT (B, 0)],
>     (C, 1.0) :-> \n -> if n>1 then [NT (C, 0), NT (C, n-1)] else [NT (C, 0)]
>     ]

> abcGen1 n = putStrLn $ concatMap show $ map fst $ toPairs $ snd $
>     gen abcRules1 (mkStdGen 0, [NT (St,n)]) !! (n+1)

Of course, the gen function can create the same patterns with an 
essentially parameter-less grammar (i.e. just a CFG) if one simply 
allows terminals for (X,0) and nonterminals for (X, n>1) and then 
selects the nth iteration for n>0, since the grammar is deterministic
and gen functions like an L-System. However, n=0 evaluating to the
empty string is not possible in this scenario.

