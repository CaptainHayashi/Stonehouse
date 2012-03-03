Rapier.Obgen.Php.Generate
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

This module is the front-facing part of the PHP code generator, which
takes an abstract Rapier object (see Rapier.Obgen.Object) and converts
it into an abstract specification of a PHP program, which can then be
passed to the PHP compiler to create a PHP code file.

> module Rapier.Obgen.Php.Generate (generatePhp) where
> import Rapier.Obgen.Php.GenComment
> import Rapier.Obgen.Php.GenClass
> import Rapier.Obgen.Php.Namespaces (rapierClassToPhpNamespace)
> import Rapier.Obgen.Php.Types
> import Rapier.Obgen.Object


Default class name
------------------

The standard PHP class name (qualified by the namespace generated from
the Rapier class name) of any generated PHP class is as follows.

> defaultClassName :: ClassName
> defaultClassName = "Object"


Top-level generation
--------------------

Given an object specification, the following function creates a PHP
class object for a PHP class implementing that specification.

> generatePhp :: ObjectSpec -> Maybe Php
> generatePhp (ObjectSpec rclass metadata fields specials) =
>     Just
>     ( PhpStatementList
>       ( makePreamble ns defaultClassName metadata ++
>         genClass ns defaultClassName metadata fields specials
>       )
>     )
>     where ns = rapierClassToPhpNamespace rclass


Preamble
--------

The preamble of a PHP Rapier class is the initial file doc comment,
the namespace, and a set of common imports.

> makePreamble :: Namespace -> ClassName -> Metadata -> [PhpStatement]
> makePreamble namespace classname metadata =
>     [ CommentStatement initialComment,
>       NamespaceStatement namespace,
>       RequireOnceStatement "helpers/object_common.php" ]
>         where
>         initialComment =
>             makeInitialComment namespace classname metadata