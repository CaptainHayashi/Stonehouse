Rapier.Obgen.Php.CompileMethod
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

This module pulls together any PHP-specific functions and constants
that are shared between the compilation and generation phases.

> module Rapier.Obgen.Php.Utils
>     ( toPhpFieldName,
>       -- :: String -> Identifier
>       toPhpIndexName,
>       -- :: String -> Identifier
>       toPhpConstName,
>       -- :: String -> Identifier
>       toPhpParamName,
>       -- :: String -> Identifier
>       toPhpAccessorName,
>       -- :: String -> Identifier
>       fieldAccess,
>       -- :: Identifier -> Identifier -> Identifier
>       rapierTypeToPhp,
>       -- :: RapierType -> PhpType
>       rapierTypeToString,
>       -- :: RapierType -> String
>       fieldKeyFor
>       -- :: String -> Identifier
>     ) where
> import Rapier.Obgen.Php.Types
> import Rapier.Obgen.ConvertName ( toCamelCase,
>                                   toUppercaseUnderscored,
>                                   toLowercaseConcatenated )
> import Rapier.Obgen.Object


Rapier field name mapping to PHP field name
-------------------------------------------

This function converts a Rapier object field name into a PHP
identifier that will represent that field in the generated code.

It does not prepend a dollar symbol (so as to make the field's
declared name), as this function is intended to be useful both when
declaring the field and referring to it from an object-orientated
perspective.

> toPhpFieldName :: String -> Identifier
> toPhpFieldName = ("_"++) . toCamelCase


Rapier field name mapping to PHP array ID name
----------------------------------------------

This conversion is mainly done to make database pulling easier, as the
convention used when performing this map is the same as used by
convention in Rapier database backends.

> toPhpIndexName :: String -> Identifier
> toPhpIndexName = toLowercaseConcatenated


Rapier field name mapping to PHP constant name
----------------------------------------------

This function converts a Rapier object field name into a PHP
identifier suitable for usage in a constant name.

> toPhpConstName :: String -> Identifier
> toPhpConstName = toUppercaseUnderscored


Making an identifier that represents a formal parameter
-------------------------------------------------------

This function takes the standard Rapier name of a field, and generates
a good name for a PHP formal parameter concerning that field.

The result of this function will ideally be similar to, but not
identical to, the result of, toPhpFieldName.

> toPhpParamName :: String -> Identifier
> toPhpParamName = toCamelCase -- Yup, that's all there is to it


Making an identifier that represents an accessor
------------------------------------------------

This function takes the standard Rapier name of a field, and generates
a good name for an accessor for that field.

> toPhpAccessorName :: String -> Identifier
> toPhpAccessorName = toCamelCase -- Nothing special here


Making an identifier that represents a field access
---------------------------------------------------

This function takes the name of an object and the name of a field
within that object, and composes an identifier representing an access
of that object's field.

For example, fieldAccess "$foo" "bar" returns "$foo->bar".

Any initial $ symbols on the field name are stripped; a lack of
initial $ symbol on the object name is corrected.

> fieldAccess :: Identifier -> Identifier -> Identifier
> fieldAccess obj ('$':xs) = fieldAccess obj xs
> fieldAccess ('$':obj) field = '$':obj ++ "->" ++ field
> fieldAccess obj field = fieldAccess ('$':obj) field


Rapier types mapping to PHP types
---------------------------------

This function takes a Rapier type literal, and returns the
corresponding PHP type literal.

> rapierTypeToPhp :: RapierType -> PhpType
> rapierTypeToPhp RpInteger       = PInteger
> rapierTypeToPhp RpNatural       = PInteger
> rapierTypeToPhp RpReal          = PDouble
> rapierTypeToPhp RpDate          = PMixed
> rapierTypeToPhp RpTime          = PInteger
> rapierTypeToPhp RpString        = PString
> rapierTypeToPhp RpBoolean       = PBool
> rapierTypeToPhp ( RpVector _ )  = PArray
> rapierTypeToPhp ( RpMap    _ )  = PArray
> rapierTypeToPhp ( RpObject _ )  = PMixed
> rapierTypeToPhp ( RpArray _ _ ) = PArray


Rapier types mapping to filter method arguments
-----------------------------------------------

Each field in a PHP realisation of a Rapier class is sent through a
filter function, which accepts a string realisation of a Rapier type.
This function maps from Rapier types to those string realisations.

> rapierTypeToString :: RapierType -> String
> rapierTypeToString RpInteger       = "Integer"
> rapierTypeToString RpNatural       = "Natural"
> rapierTypeToString RpReal          = "Real"
> rapierTypeToString RpDate          = "Date"
> rapierTypeToString RpTime          = "Time"
> rapierTypeToString RpString        = "String"
> rapierTypeToString RpBoolean       = "Boolean"
> rapierTypeToString ( RpObject a )  = "Object:" ++ a
> -- Recursive types
> rapierTypeToString ( RpVector a )  =
>     '<' : rapierTypeToString a ++ ">"
> rapierTypeToString ( RpMap    a )  =
>     '{' : rapierTypeToString a ++ "}"
> rapierTypeToString ( RpArray n a ) =
>     '[' : show n ++ "x" ++ rapierTypeToString a ++ "]"


Array key constant for a given field
------------------------------------

> fieldKeyFor :: String -> Identifier
> fieldKeyFor = ( "self::FIELD_" ++ ) . toPhpConstName
