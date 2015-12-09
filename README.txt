Kulitta version 2.0.1.06
Donya Quick
Last modified 08-December-2015

NOTE: this version of the code uses Euterpea 1.1 and UISF 4.0 
on Hackage. It will NOT work with Euterpea 1.0 or UISF <4. 
If you have older versions of Euterpea already installed, you
can update them by doing this:

cabal update
cabal install Euterpea

If you get warnings about reinstalling, you can use this:

cabal install Euterpea --reinstall --force-reinstalls

Kulitta was the subject of my doctoral dissertation titled
"Kulitta: a Framerwork for Automated Composition" published 
Deceber 2014. Kulitta is a subject of ongoing research and 
therefore continues to be updated over time.

Kulitta 1.1 represents a Euterpea 1.0 compatible version of 
the doctoral thesis code - it is very close to what is printed
in the dissertation itself. 

Kulitta 2.0.0 has many additioanl changes to various parts of 
Kulitta, particularly to the types and other representations 
used for Probabilistic Temporal Graph Grammars (PTGGs). The 
PTGG implementation has been "un-monaded" for easier control 
over random threading and is also now more general (and 
therefore more powerful) such that it can be used for both
musical and non-musical alphabets more easily. 

Kulitta 2.0.1 features updates to the graphical interface 
and requires the newer Euterpea 1.1.0 and UISF 4.0.0. 

See http://www.donyaquick.com/kulitta for more information.

To use Kulitta:

1. Install Haskell Platform: https://www.haskell.org/platform/
   Please use 32-bit Haskell Platform 7.10.2 if the option 
   exists for your operating system, otherwise please use 
   32-bit Haskell Platform 2014. Euterpea, a library that 
   Kulitta uses, is not compatible with 64-bit versions of 
   Haskell Platform.

2. Install Euterpea. Open a command prompt or terminal and 
   run: cabal install Euterpea-1.1.0

3. Load any of the modules in Kulitta as described below.

Notable source files:

Kulitta interactive GUI
	Kulitta.lhs
	Compiled for Windows as Kulitta.exe

Vesicularia (algorithmic composition example)
	Vesicularia.lhs

Learning algorithm (see sampleConfig folder for example learning configuration)
	LearningMain.lhs
	Learning.lhs