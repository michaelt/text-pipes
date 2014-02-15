pipes-text
==========

This package follows the rule `pipes-text : pipes-bytestring :: text : bytestring` It has three modules, `Pipes.Text` , `Pipes.Text.Encoding` and `Pipes.Text.IO`; the division has more or less the significance it has in the `text` library. 

Note that the module `Pipes.Text.IO` uses version 0.11.3 or later of the `text` library. (It thus works with the version of `text` that came with the 2013 Haskell Platform. To use an older `text`, install with the flag `-fnoio` 