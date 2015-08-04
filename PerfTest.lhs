> module PerfTest where
> import ChordSpaces

This is a simple demonstration of why it is better to enforce
single-chord constraints at the set level before even forming
a quotient space. The examples take long enough to finish that 
the last-modified time of the files can be used as a rough 
estimate of runtime. The first test, testA, will complete 
relatively quickly (1 minute or so on a recent machine). 
On the other hand, testB will take a long time to complete.

> f = r [0,4,7]
> s = makeRange (take 3 $ repeat (0,88))
> r = optEq

> val1 = filter f s // r
> val2 = map (filter f) (s // r)

> testA = writeFile "fooA1.txt" "foo" >> putStrLn (show val1) >>
>         writeFile "fooA2.txt" "foo"

> testB = writeFile "fooB1.txt" "foo" >> putStrLn (show val2) >>
>         writeFile "fooB2.txt" "foo"