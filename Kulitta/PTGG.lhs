Probabilistic Temporal Graph Grammar Implementation
Donya Quick
Last modified: 20-Dec-2014

Based on the original monadic implementation by Donya Quick and Paul Hudak 
(see ICMC 2013 and FARM 2013 papers). 

Major changes:
- Removal of the S constructor for sentences.
- Removal of Mod constructor. Modulation information will be stored as a parameter.
- Chord constructor renamed to MSym to allow for PTGGs over other features.

> module Kulitta.PTGG where
> import Data.List
> import System.Random

The Term data structure has three constructors: 
    1. NT - a nonterminal that has a symbol and a parameter.
    2. Let - a means of capturing variable instantiation in a Term.
    3. Var - a variable	
	
> data Term a b = NT (a,b) | Let String (Sentence a b) (Sentence a b) | Var String
>      deriving (Eq, Show)

A Sentence is a list of Terms.

> type Sentence a b = [Term a b]

A rule has a left and righthand side. The lefthand side has
an un-parameterized symbol and a probability for application of
the rule. The righthand side is a function from a parameter to
a Sentence.

> type Prob = Double
> data Rule a b = (a, Prob) :-> RuleFun a b
> type RuleFun a b = b -> Sentence a b


A function to rewrite one Sentence to another using an 
L-System-like approach to generation where all symbols are 
updated from left to right.

> update :: (Eq a, Eq b) => [Rule a b] -> (StdGen, Sentence a b) -> (StdGen, Sentence a b)
> update rules (g, []) = (g,[])
> update rules (g,(t:ts)) = case t of
>     NT (c,d)  -> let (g1, t') = applyRule rules g (c,d)
>                      (g2, ts') = update rules (g1, ts) 
>                  in  (g2, t'++ts')
>     Let x a e -> let (g1,a') = update rules (g, a)
>                      (g2,e') = update rules (g1, e)
>                      (g3,ts') = update rules (g2, ts)
>                  in  (g3, Let x a' e' : ts')
>     x         -> let (g1, ts') = update rules (g, ts)
>                  in  (g1, x : ts')

Function to update a single symbol:

> applyRule :: (Eq a) => [Rule a b] -> StdGen -> (a,b) -> (StdGen, Sentence a b)
> applyRule rules g (c,d) =  
>   let  rs = filter (\((c',p) :-> rf) -> c'==c) rules
>        (p,g') = randomR (0.0::Double, 1.0) g
>   in   if null rs then (g, [NT (c,d)]) else (g', choose rs p $ d) where -- See note below
>       choose :: [Rule a b] -> Prob -> (RuleFun a b)
>       choose [] p  = error "Nothing to choose from!"
>       choose (((c,p') :-> rf):rs) p  = 
>           if p<=p' || null rs then rf else choose rs (p-p') 

Note: we assume the value is a NT (nonterminal) because applyRule can only
be called from NT in update. We want to leave these as NTs rather than forcing
them to terminals in case the user wishes to apply different rule sets later.

User-level generation:

> gen :: (Eq a, Eq b) => [Rule a b] -> (StdGen, Sentence a b) -> [(StdGen, Sentence a b)]
> gen rules (g,t) = iterate (update rules) (g,t)

The expand function eliminates Lets and Vars from a generated Term a.
It allows for nested Let expressions for variables with the same name
with lexical scoping. For example:

expand [] [Let "x" t1 [Let "x" t2 (Var "x")]] ==> t2

> expand :: [(String, Sentence a b)] -> Sentence a b -> Sentence a b
> expand e [] = []
> expand e (t:ts) = case t of 
>     Let x a exp -> expand ((x, expand e a) : e) exp ++ expand e ts
>     Var x       -> (maybe (error (x ++ " is undefined")) id $ lookup x e) ++ expand e ts
>     x           -> x : expand e ts

------------------------------------------------------------------------

Additional manipulations

Map defined over Term to convert pairs of one type to pairs of another.

> tMap :: ((a,b) -> (c,d)) -> Sentence a b -> Sentence c d
> tMap f [] = []
> tMap f (t:ts) = 
>     let t' = case t of
>                  Let x a exp -> Let x (tMap f a) (tMap f exp)
>                  Var x -> Var x
>                  NT x -> NT $ f x
>     in  t' : tMap f ts

Flattening completely to a list

> toPairs :: Sentence a b -> [(a, b)]
> toPairs = map f . expand [] where
>     f (NT (ct, d)) = (ct,d) 
>     f _ = error "(toPairs) Variable or Let expression encountered"

Reassigning probabilities

> updateProbs :: [Rule a b] -> [Prob] -> [Rule a b]
> updateProbs rs ps = 
>     if length rs /= length ps then error "(updateProbs) Probability/rule count mismatch."
>     else zipWith (\((l,_):->rhs) p -> (l,p):->rhs) rs ps

Accessing the various portions of a rule without pattern matching

> lhs ((c,p) :-> rf) = c
> prob ((c,p) :-> rf) = p
> rfun ((c,p) :-> rf) = rf

Normalize the probabilities for a rule set

> normalize :: (Eq a) => [Rule a b] -> [Rule a b]
> normalize [] = []
> normalize (r@((l,p) :-> rf):rs) = 
>     let rset = r : filter ((l==).lhs) rs
>         rset' = filter ((l/=).lhs) rs
>         psum = sum $ map prob rset
>     in  map (\((l',p') :-> c') -> ((l',p'/psum) :-> c')) rset ++ normalize rset'
