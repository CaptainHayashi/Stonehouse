Rapier.Obgen.Php.CompileCommon
==============================

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

This module collates together some small functions used across many
different parts of the PHP abstract->concrete compiler.

> module Rapier.Obgen.Php.CompileCommon
>     ( compileVisibility,
>       -- :: Visibility -> String
>       indent,
>       -- :: String -> String
>       compileParams,
>       -- :: ( a -> String ) -> [ a ] -> String
>       typeName
>       -- :: PhpType -> String
>     ) where
> import Rapier.Obgen.Php.Types
> import Data.List ( intercalate )


Visibility compilation
----------------------

Compiling a visibility is an exercise in basic pattern matching.

> compileVisibility :: Visibility -> String
> compileVisibility Public    = "public"
> compileVisibility Protected = "protected"
> compileVisibility Private   = "private"


Indenting
---------

This function indents every line in the given string by four spaces,
by splitting the string into a list its constituent lines, getting the
list resulting from prepending four spaces to each line, and merging
them all back together.

> indent :: String -> String
> indent = concatMap ( ("    "++ ) . ( ++"\n" ) ) . lines


Parameter lists
---------------

This is a general parameter list constructor, taking a function that
handles the individual parameters.  This came about because the
general form of a parameter/argument list is the same in both
function signatures and function application, but there are some
additional nuances to signature parameter lists that don't exist in
application argument lists.

In the interests of prettiness, there are two special cases, for when
there are no params or when there is only one param.

> compileParams :: ( a -> String ) -> [ a ] -> String
> compileParams _ [] = "()"
> compileParams f ( param:[] ) = '(' : f param ++ ")"
> compileParams f params = "(\n" ++ comp params ++ ")"
>     where
>     comp = indent . intercalate ",\n" . map f


Compiling PHP types
-------------------

This is a simple pattern matching exercise used to translate PHP data
types into the strings that are used to identify them in concrete PHP.

> typeName :: PhpType -> String
> typeName PString       = "string"
> typeName PInteger      = "integer"
> typeName PBool         = "bool"
> typeName PCallback     = "callback"
> typeName PDouble       = "double"
> typeName PMixed        = "mixed"
> typeName PArray        = "array"
> typeName ( PObject a ) = a
