pipes-text
==========

These modules `Pipes.Text` and `Pipes.Text.Parse` use materials from [`pipes-text`](https://github.com/ibotty/pipes-text) and 
otherwise follows the pattern of [`pipes-bytestring`](https://github.com/Gabriel439/Haskell-Pipes-ByteString-Library), adding a few `pipes-prelude`-like operations.
The most important function, `decodeUtf8`, written by ibotty, uses the development version of the text package; this package can however be built without it.

     >>> runEffect $ stdinLn >-> P.takeWhile (/= "quit") >-> stdoutLn
     hi<Return>
     hi
     quit<Return>
     >>> runSafeT $ runEffect $ readFile "README.md" >-> map toUpper >-> hoist lift stdout
     PIPES-TEXT
     ==========
     ...