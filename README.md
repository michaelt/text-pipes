text-pipes
==========

This repo is called `text-pipes`, but the package is named `pipes-text` as one might expect.  
The two modules it contatins, `Pipes.Text` and `Pipes.Text.Parse`, use materials from [`pipes-text`](https://github.com/ibotty/pipes-text); 
otherwise they follow the pattern of [`pipes-bytestring`](https://github.com/Gabriel439/Haskell-Pipes-ByteString-Library), adding a few `pipes-prelude`-like operations.
The most important function, `decodeUtf8`, written by ibotty, uses the development version of the text package; this package can however be built with the hackage `text` 
though `decodeUtf8` will then not exist.

     >>> runEffect $ stdinLn >-> P.takeWhile (/= "quit") >-> stdoutLn
     hi<Return>
     hi
     quit<Return>
     >>> runSafeT $ runEffect $ readFile "README.md" >-> toUpper >-> hoist lift stdout
     TEXT-PIPES
     ==========
     ...