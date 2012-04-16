Rapier.Obgen.Php.GenClass
=========================

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

This module is responsible for generating code for converting a Rapier
object between its PHP object format and its PHP array (key-value map)
format.

> module Rapier.Obgen.Php.GenArrayConverters
>     ( genToArray,
>       -- :: Identifier -> PhpClassStatement
>       genFromArray
>       -- :: ClassName -> [ Identifier ] -> PhpClassStatement
>     ) where
> import Rapier.Obgen.Php.Types
> import Rapier.Obgen.Php.GenComment
> import Rapier.Obgen.Php.Utils
>     ( toPhpAccessorName
>     , fieldAccess
>     , fieldKeyFor
>     )
> import Rapier.Obgen.Utils
>     ( ( &:& )
>     )


To-array converter
------------------

This function writes the code that packs the object up into an array.
Said code simply consists of a returning of an array mapping the
FIELD_xyz constant keys to the results of calling the respective
accessors.

> genToArray :: [ Identifier ] -> [ ClassStatement ]
> genToArray = comment &:& meth
>     where
>     comment = ClassComment . toArrayComment
>     meth fnames = publicInstMethod "toArray" []
>                   [ Return ( ArrayExpr items ) ]
>         where
>         items = map fieldToItem fnames
>         fieldToItem name =
>             StaticAccess "self" ( fieldKeyFor name ) :=>:
>             FunctionCallExpr
>             ( fieldAccess "this"
>               ( toPhpAccessorName name )
>             ) []


From-array converter
--------------------

The from-array converter does literally two things: it checks to see
if there are items in the incoming array with labels matching the
array indices associated with each field, then it sends the results to
the constructor which does everything else.


> genFromArray :: ClassName -> [ Identifier ] -> [ ClassStatement ]
> genFromArray cn fnames = [ comment, meth ]
>     where
>     comment = ClassComment ( makeFromArrayComment cn )
>     meth = publicStaticMethod "fromArray" [ PArray :$ "array" ]
>            ( makeFromArrayChecks fieldIndices ++
>              makeFromArrayReturn cn fieldIndices
>            )
>     fieldIndices = map fieldKeyFor fnames

***

The first part of the from-array converter is created by
makeFromArrayChecks, which simply constructs a call for each field
index to a library function that'll throw an exception if the given
array is missing that index.

(Note that we only check that the relevant array fields exist; there's
no point checking types here, as it's done later in the object
constructor.  Given the amount of effort Rapier puts into filtering
the input at the constructor level, there's no sense in duplicating
it!  As such, we ignore the type.)

> makeFromArrayChecks :: [ Identifier ] -> [ MethodStatement ]
> makeFromArrayChecks = map makeCheck
>     where
>     makeCheck index =
>         NakedExpr
>         ( FunctionCallExpr
>           "\\URY\\API\\Helpers\\fail_if_not_in_array"
>           [ IdExpr "$array", StaticAccess "self" index ]
>         )

***

The second part then returns the result of constructing a new instance
of the class with the array items piped in as the parameters.  It
requires the class name in addition to the field indices, so that the
invocation of new knows which class to make.

> makeFromArrayReturn :: ClassName -> [ Identifier ]
>                        -> [ MethodStatement ]
> makeFromArrayReturn cn indices = [ Return ( New cn params ) ]
>     where params = map indexToParam indices
>           indexToParam =
>               ArraySubscript "$array" . StaticAccess "self"
