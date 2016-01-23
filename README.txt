Kulitta: a Library for Automated Music Composition
(c) Donya Quick 2014-2016
Version: 2.2.0

Kulitta is a framework for automated composition that 
can also be configured to run as a standalone AI for 
generating music in a particular style. 

This library is not intended for commercial use.

For more information on Kulitta, go to:
http://www.donyaquick.com/kulitta

Note: Kulitta's graphical interface is now provided
as an example use of the larger Kulitta library. It
is located in the Examples\GUI folder in GUI.lhs. 


======================

INSTALLATION INSTRUCTIONS

To use Kulitta, you will need Haskell Platform. 
Please use Haskell Platform 2014 or 7.10.13. Note 
that Kulitta's GUI may not work on Macs running 
7.10.3 but should work with Haskell Platform 2014.

This version of the code uses Euterpea 2.0 from 
GitHub. If you want to use Kulitta's GUI, you will 
also need the HSoM library. See euterpea.com for 
information on installing these libraries.

Once you have installed these, run "cabal install" 
from within the library folder (where kulitta.cabal 
is located). You can then use Kulitta by importing
the following:

Kulitta
    Contains the PTGG, ChordSpaces, PostProcessing,
    Search, and Constraints modules. These can also 
    be imported individually if desired.
    
Kulitta.Grammars.MusicGrammars
    Some musical grammars derived from music theory
    and recent publications on the topic.
    
Kulitta.Foregrounds
    All of Kulitta's foreground modules. This includes
    ClassicalFG, JazzFG, and SimplePianoFG. These can
    also be imported individually if desired.
    
Kulitta.Learning.Learning
    Kulitta's learning module, intended to be compiled
    and run separately as in LearningMain.lhs. 
    
Please consult individual lhs files within the library
for documentation on the functions within.

