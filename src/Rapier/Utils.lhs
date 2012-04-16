Rapier.Utils
============

Part of the Haskell Rapier Common Library

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

This module contains various utility functions for Rapier that should
probably find a good home at some point.

> module Rapier.Utils
>     ( escape     -- Char -> String -> String
>     , unescape   -- Char -> String -> String
>     , safeHead   -- [ a ] -> Maybe a
>     , onHead     -- ( a -> a ) -> [ a ] -> [ a ]
>     )
> where
> import Data.List
>     ( foldl'
>     )

> escape :: Char -> String -> String
> escape c = foldl' ( flip escapeInner ) ""
>     where
>     escapeInner x
>         | x == c    = ( ++ ( '\\' : [ x ] ) )
>         | otherwise = ( ++          [ x ]   )

> unescape :: Char -> String -> String
> unescape c = foldl' unescapeInner ""
>     where
>     unescapeInner current x
>     -- If we saw a \ last and a character to escape comes up,
>     -- effectively remove the \
>         | x == c && last current == '\\' =
>             init current ++ [ x ]
>         | otherwise = current ++ [ x ]


Safe head functions
-------------------

safeHead, on a list (x:xs), returns Just x; on the empty list it
returns Nothing.

> safeHead :: [ a ] -> Maybe a
> safeHead []        = Nothing
> safeHead ( x : _ ) = Just x


onHead is an function, that applies a given function to the head of a
list if said list is non-empty.

> onHead :: ( a -> a ) -> [ a ] -> [ a ]
> onHead _ []         = []
> onHead a ( x : xs ) = a x : xs