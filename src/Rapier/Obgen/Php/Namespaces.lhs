Rapier.Obgen.Php.Namespaces
===========================

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

This module provides functions and definitions relating to PHP
namespaces, and the generation of them from Rapier class names.

> module Rapier.Obgen.Php.Namespaces
>     ( rapierClassToPhpNamespace -- :: String -> Namespace
>     , qualifyPhpClassName       -- :: ClassName -> ClassName
>     )
> where
> import Rapier.Obgen.Php.Constants
>     ( mainNamespace
>     )
> import Language.Php.Syntax
> import Data.Text
>     ( Text
>     , pack
>     , split
>     , empty
>     )


Rapier class-names mapping to PHP namespaces
--------------------------------------------

This function takes a Rapier class-name, and returns the corresponding
PHP namespace.

The PHP namespace is defined as the result of prefixing the Rapier
classname with all / replaced with \ with the namespace prefix plus
another \ (to glue the two together).

> rapierClassToPhpNamespace :: Text -> Namespace
> rapierClassToPhpNamespace =
>     spliceNamespace mainNamespace . ( delimTextToNamespace '/' )

***

The function delimTextToNamespace takes text and a one-character
delimiter, chops the text up at that delimiter, and massages the
resultant list into a namespace starting with the 

If the string starts with the separator, the result will be an
absolute namespace; if not, then it will be a relative namespace.

> delimTextToNamespace :: Char -> Text -> Namespace
> delimTextToNamespace = textListToNamespace `comp2` split
>     where comp2 = (( . ) . ( . ))

> textListToNamespace :: [ Text ] -> Namespace
> textListToNamespace lst@( x : xs )
>     | x == empty = foldl ( :\ ) AbsoluteRoot xs
>     | otherwise  = foldl ( :\ ) RelativeRoot lst

***

spliceNamespace splices one namespace onto the end of another.

If the end namespace is absolute, the resultant namespace will just be
the end namespace; if not, then it is placed into the context of the
start namespace.

> spliceNamespace :: Namespace -> Namespace -> Namespace
> spliceNamespace start end
>     | namespaceRoot end == AbsoluteRoot = end
>     | otherwise = replaceRootWith end start

> namespaceRoot :: Namespace -> Namespace
> namespaceRoot ( ns :\ _ ) = namespaceRoot ns
> namespaceRoot root = root

> replaceRootWith :: Namespace -> Namespace -> Namespace
> replaceRootWith ( AbsoluteRoot ) to = to
> replaceRootWith ( RelativeRoot ) to = to
> replaceRootWith ( ns :\ str ) = ( replaceRootWith ns ) :\ str


Qualifying class names
----------------------

The following function takes an unqualified (relative) PHP class name
(such as "Object") and converts it to a qualified (absolute) class
name under the Rapier namespace (such as "\URY\API\Object").

Any absolute names passed in (those preceded by a backslash) are
returned unmolested.

> qualifyPhpClassName :: ClassName -> ClassName
> qualifyPhpClassName ('\\':xs) = '\\':xs
> qualifyPhpClassName relative =
>     '\\':mainNamespace ( "\\" ++ relative )