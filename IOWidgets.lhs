Basic IO Widgets
Donya Quick
Last modified 26-Mar-2015

> module IOWidgets where
> import Euterpea
> import FRP.UISF.UISF

An ioWidget1 takes an IO operation over a value and
creates a UISF widget that will call that IO operation
whenever Just is encountered in an event stream. For 
example, you can use this to save a file on a button 
press.
 
> ioWidget1 :: (a -> IO ()) -> UISF (SEvent a) ()
> ioWidget1 = (>>> arr (const ())) . uisfSinkE

An example of saving a text file using this widget:

fileSaver :: UISF (SEvent (FilePath, String)) ()
fileSaver = ioWidget1 (uncurry writeFile)

An ioWidget2 has more functionality and allows input
from IO into a stream value in addition to the output.
It takes a default output (which can be Nothing if the
desired output is an event stream) and an IO operation
to perform triggered on an event stream. The result of 
that IO operation is returned as a stream.

> ioWidget2 :: b -> (a -> IO b) -> UISF (SEvent a) b
> ioWidget2 def = (>>> arr (maybe def id)) . uisfPipeE

An example of using this to read from a file:

fileReader :: UISF (SEvent FilePath) (SEvent String)
fileReader = ioWidget2 Nothing readIt where
    readIt filePath = do
        x <- readFile filePath
        return (Just x)

