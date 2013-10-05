text-pipes
==========

Text pipes, somehow to be fused with [`pipes-text`](https://github.com/ibotty/pipes-text) 
which uses an upcoming version of the `text` library to link bytestring and text.

This module follows the pattern of `pipes-bytestring` (largely by skillful use of the
expedient of regular expressions), and adds a few `pipes-prelude`-like operations.


     >>> runEffect $ stdinLn >-> P.takeWhile (/= "quit") >-> stdoutLn
     hi<Return>
     hi
     quit<Return>
     >>> runSafeT $ runEffect $ readFile "README.md" >-> map toUpper >-> hoist lift stdout
     TEXT-PIPES
     ==========
     TEXT PIPES, SOMEHOW TO BE FUSED WITH `PIPES-TEXT`.
     ...