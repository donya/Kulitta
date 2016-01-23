Main module for Learning
Donya Quick

Last modified: 22-Jan-2016

A GHC-compilable interface to Learning.lhs

To compile, use:

ghc -O2 LearningMain.lhs -o "Learning.exe" -rtsopts -threaded

To run, use:

Learning.exe configFile.txt +RTS -Nx

where x is the number of cores you wish to use (e.g. use -N8 for 8 cores).

See the sampleConfig folder for examples of configuration files. 

> module Main where
> import Learning

> main = mainL