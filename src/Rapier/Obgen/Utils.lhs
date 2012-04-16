Rapier.Obgen.Utils
==================

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

This module collects together some useful utility functions.

> module Rapier.Obgen.Utils
>     ( lowerCaseInitial -- String -> String
>     , upperCaseInitial -- String -> String
>     , enquote          -- String -> String
>     , doubleEnquote    -- String -> String
>     , pairToList       -- ( a, a ) -> [ a ]
>     , ( &:& )          -- Arrow a => a b c -> a b c -> a b [ c ]
>     )
> where
> import Data.Char
>     ( toLower
>     , toUpper
>     )
> import Control.Arrow
>     ( Arrow
>     , ( &&& )
>     , ( ^<< )
>     )


Changing the case of the initial letter of a string
---------------------------------------------------

lowerCaseInitial and upperCaseInitial are two very similar functions
that change the initial letter of a string to lower-case or upper-case
respectively.

They are both defined in terms of a general function modifyInitial,
which applies an a->a function on the initial item of a list of a.

> lowerCaseInitial :: String -> String
> lowerCaseInitial = modifyInitial toLower

> upperCaseInitial :: String -> String
> upperCaseInitial = modifyInitial toUpper

> modifyInitial :: (a -> a) -> [a] -> [a]
> modifyInitial _ [] = []
> modifyInitial f (x:xs) = f x : xs


Enquoting strings
-----------------

enquote encapsulates a string in single quotes; doubleEnquote
encapsulates in double quotes.

> enquote, doubleEnquote :: String -> String
> enquote = ( "'"++ ) . ( ++"'" )
> doubleEnquote = ( "\""++ ) . ( ++"\"" )


Pairs to lists
--------------

> pairToList :: ( a, a ) -> [ a ]
> pairToList ( x, y ) = [ x, y ]


&:& is like &&&, in that it performs its two arrow arguments on a
given parameter, but it returns the results as a list rather than as a
tuple.  It's useful when sending one variable to two functions that
you then want in a list form (ie list of statements when compiling).

> ( &:& ) :: Arrow a => a b c -> a b c -> a b [ c ]
> ( &:& ) = pairToList `comp` ( &&& )
>     where comp = ( ^<< ) . ( ^<< ) . ( ^<< )
>     -- I kid you not