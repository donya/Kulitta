Special playback functions
Donya Quick
Last modified: 01-July-2015

Kulitta-specific playback functions.

Most of these functions are slight modifications of Euterpea's
functions in ExperimentalPlay and MidiIO.

> module PlayK where
> import Euterpea
> import Codec.Midi
> import Sound.PortMidi
> import System.IO.Unsafe (unsafePerformIO)
> import Euterpea.IO.MIDI.MidiIO
> import System.Directory


Play a MIDI file to a user-specified device. Unlike the regular play function,
this version also checks whether the file exists to avoid crashing the calling
program. If the file doesn't exist, an error message will be printed.

> play' fp devID = do
>   x <- tryImportFile fp 
>   seq x $ case x of Left err -> putStrLn ("Error: "++err) >> return ()
>                     Right m -> playM' devID m

> playX fp devID chanOffset vol = do
>   x <- tryImportFile fp 
>   seq x $ case x of Left err -> putStrLn ("Error: "++err) >> return ()
>                     Right m -> playM' devID (trackMod chanOffset vol m)

> writeX fp m chanOffset = 
>   let x = trackMod 0 (-1) $ toMidi m 
>   in  exportMidiFile fp x


The trackMod function takes a channel offset, x, and a list of tick-stamped messages
and offsets the track numbers by x amount. The purpose of this was really a single 
use case: running the Kulitta GUI alongside other programs that used tracks 1 through
(x-1) so that Kulitta's playback would not "step on the toes" of the other programs'
playback. 

> trackMod x vol m = 
>     let t = tracks m
>         t' = map (map (trackMod' x vol)) t 
>     in  m{tracks = t'} 

> trackMod' :: Channel -> Double -> (Ticks, Message) -> (Ticks, Message)
> trackMod' x vol (a, NoteOff c k v) = (a, NoteOff (c+x) k (if vol <0 then v else volMod v vol))
> trackMod' x vol (a, NoteOn c k v) = (a, NoteOn (c+x) k (if vol <0 then v else volMod v vol))
> trackMod' x vol (a, ProgramChange c p) = (a, ProgramChange (c+x) p)
> trackMod' x vol (a, ControlChange c v1 v2) = (a, ControlChange (c+x) v1 v2)
> trackMod' x vol (a,v) = (a,v)

> volMod v vol = floor(fromIntegral v * vol)

> playF fp fmid devID = do
>   x <- tryImportFile fp 
>   seq x $ case x of Left err -> putStrLn ("Error: "++err) >> return ()
>                     Right m -> playM' devID (fmid m)

Code to check whether the file exists:

> tryImportFile fp = do
>     let theDir = getDir fp
>         theFile = getFile fp
>     files <- getDirectoryContents theDir
>     if elem theFile files then importFile fp >>= return 
>         else return $ Left ("File "++fp ++ " does not exist, so it cannot be played!") where
>         getDir fp = reverse $ dropWhile (not.(`elem` "/\\")) $ reverse fp
>         getFile fp = drop (length $ getDir fp) fp

=====================

DATA MANIPULATION FUNCTIONS

For playback, it's useful to be able to set the volume on some systems.

> setVol :: Int -> Music Pitch -> Music1
> setVol v = mMap (\p -> (p, [Volume v])) 

> setVol1 :: Int -> Music1 -> Music1
> setVol1 v = mMap (\(p,nas) -> (p, Volume v : filter f nas)) where
>     f (Volume v) = False
>     f _ = True

Sometimes it is also useful to play to devices with instrument
information stripped (i.e. sending to a single synth such that 
only one channel can be used).

> stripInstrs :: Music a -> Music a
> stripInstrs (a :+: b) = stripInstrs a :+: stripInstrs b
> stripInstrs (a :=: b) = stripInstrs a :=: stripInstrs b
> stripInstrs (Prim p) = Prim p
> stripInstrs (Modify (Instrument i) m) = stripInstrs m
> stripInstrs (Modify c m) = Modify c $ stripInstrs m

> m2m :: (Music1 -> Music1) -> Midi -> Midi
> m2m fm1 midi = 
>     let  m1 = fromMidi midi
>     in   toMidi $ perform $ fm1 m1

> ch1 :: Int -> Midi -> Midi
> ch1 v = if v <=0 then m2m stripInstrs
>         else m2m (setVol1 v . stripInstrs)