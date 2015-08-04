Euterpea import module
No new definitions - just to avoid naming conflicts with Kulitta
for small features like type synonyms that do not warrant importing
Euterpea to use.

> module EuterpeaSpecial (module Euterpea) where
> import Euterpea hiding (C, D, P, Phrase, wn, hn, qn, en, sn, tn, 
>                         Major, Minor, Mode, Dur, dur, MP, key, mode,
>                         AbsPitch)