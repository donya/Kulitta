PCFG to PTGG Conversion
Donya Quick

Last modified: 19-Decr-2014

Module for turning a PCFG into a PTGG for use with Kulitta's 
generative algorithms.

> module PCFGtoPTGG where
> import EuterpeaSpecial
> import MusicGrammars
> import Parser
> import PTGG

> toTerm :: MP -> Dur -> a -> Term a MP
> toTerm p d x = NT (x, p{dur=d})

> s' = S :: RTerm

Make a PTGG using constant durations:

> ptggRule1 :: Dur -> (Double, Parser.Rule a ) -> PTGG.Rule a MP
> ptggRule1 dConst (p, (lhs,rhs)) = 
>     (lhs,p) :-> \p -> map (toTerm p dConst) rhs where

> toPTGG1 :: Dur -> [(Double, Parser.Rule a )] -> [PTGG.Rule a MP]
> toPTGG1 d = map (ptggRule1 d)


Make a PTGG using temporal divisions of 2 and 4 and a minimum duration:

> ptggRule2 :: (Double, Parser.Rule a ) -> PTGG.Rule a MP
> ptggRule2 (a, (lhs, rhs)) = (lhs,a) :-> \par -> durPats par rhs where
>     durPats p xs = case xs of 
>         [x] -> [NT (x, p)]
>         [x1,x2] -> map NT $ zip [x1,x2] [h p, h p]
>         [x1,x2,x3] -> map NT $ zip [x1,x2,x3] [q p, q p, h p]
>         [x1,x2,x3,x4] -> map NT $ zip [x1,x2,x3, x4] [q p, q p, q p, q p]
>         _ -> error ("(toPTGG2) Bad rule rank: "++show (length xs))

> toPTGG2 :: (Dur -> Bool) -> [(Double, Parser.Rule a )] -> [PTGG.Rule a MP]
> toPTGG2 fd = map (toRelDur fd . ptggRule2)


Make a PTGG Using only CTypes

> ptggRule3 :: (Double, Parser.Rule RTerm ) -> PTGG.Rule CType MP
> ptggRule3 (d, (lhs, rhs)) = ptggRule2 (d, (forceCT lhs, map forceCT rhs))

> toPTGG3 :: (Dur -> Bool) -> [(Double, Parser.Rule RTerm )] -> [PTGG.Rule CType MP]
> toPTGG3 fd = normalize . map (toRelDur2 fd . ptggRule3)




Utility for forcing conversion of generated Terms. TR and T are forced to
I, DR and D are forced to V, and so on. P in this case is "plagal" (which 
resolves to I), but this interpretation would also work for P as "phrase."

> forceCT :: RTerm -> CType
> forceCT (C ct) = ct
> forceCT r = 
>     let rts = [Piece, TR, DR, SR, T, D, s', T, TP, TCP, SP, DP, P] :: [RTerm]
>         cts = [I,     I,  V,  IV, I, V, IV, I, I,  I,   IV, V,  I] :: [CType] 
>         rcts = zip rts cts :: [(RTerm, CType)]
>     in  case lookup r rcts of Just y -> y
>                               Nothing -> error "(forceCT) Unhandled constructor" 
