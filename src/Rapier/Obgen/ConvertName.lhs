Rapier.Obgen.Php.Compile
========================

Part of Stonehouse, the Rapier Object Code Generator

Copyright (c) 2012, University Radio York
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.
    * Neither the name of University Radio York nor the
      names of its contributors may be used to endorse or promote
      products derived from this software without specific prior
      written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL UNIVERSITY
RADIO YORK BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

***

Licencing mumbo-jumbo aside...

This module collates functions that involve converting names,
primarily from Rapier-convention "Sentence of natural English words"
to concrete language conventions such as lowerInitialCamelCase and
ALL_CAPITALS_UNDERSCORED.

> module Rapier.Obgen.ConvertName
>     ( toCamelCase,
>       -- :: String -> String
>       toUppercaseUnderscored,
>       -- :: String -> String
>       toLowercaseConcatenated
>       -- :: String -> String
>     ) where
> -- Used for toCamelCase
> import Rapier.Obgen.Utils ( lowerCaseInitial,
>                             upperCaseInitial )
> import Data.Char ( toLower,
>                    toUpper )


Sentence to camelCase
---------------------

This function converts a sentence into its camelCased equivalent.  It
uses the following sequence of transformations:

1. Splits the sentence into a list of its words (delimited by spaces);
2. Uppercases the first character of each word;
3. Concatenates the words into one big word (with no spaces);
4. Lowercases the first character of the result.

> toCamelCase :: String -> String
> toCamelCase = lowerCaseInitial . concatMap upperCaseInitial . words


Sentence to UPPERCASE_UNDERSCORED
---------------------------------

This function converts a sentence into its UPPERCASED_UNDERSCORED
equivalent.

> toUppercaseUnderscored :: String -> String
> toUppercaseUnderscored = map (replaceSpace . toUpper)
>     where
>     replaceSpace ' ' = '_'
>     replaceSpace a   = a


Sentence to lowercaseconcatenated
---------------------------------

This function converts a sentence into its lowercaseconcatenated
equivalent.

> toLowercaseConcatenated :: String -> String
> toLowercaseConcatenated = concat . words . map toLower