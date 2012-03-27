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

This module takes a /generated/ abstract representation of a PHP
program implementing a Rapier class, and produces from it a concrete
PHP code listing which can then be used in a PHP implementation of the
Rapier system.

NOTE: Each statement compiler is responsible for adding its own
newlines.

> module Rapier.Obgen.Php.Compile ( compile ) where
> import Rapier.Obgen.Php.Types
> import Rapier.Obgen.Php.CompileCommon
> import Rapier.Obgen.Php.CompileComment
> import Rapier.Obgen.Php.CompileMethod
> import Rapier.Obgen.Php.CompileExpr


Top-level compiler
------------------

The compiler, at the highest level, compiles a PHP program into a
string of concrete code.  It wraps the result of an inner statement
list compiler (which simply joins all the results of compileStm on
each statement with newlines) in PHP opening and closing tags.

> compile :: Php -> String
> compile ( PhpStatementList stms ) =
>     concat [ "<?php\n", compileStms stms, "?>" ]
>         where
>         compileStms = concatMap compileStm


Statement compiler
------------------

This function is responsible for compiling outer statements (those
that aren't inside a class).  It mainly delegates to other functions.

> compileStm :: PhpStatement -> String

Compiling comments is covered in the CompileComment module.

> compileStm ( CommentStatement comment ) =
>     compileComment comment
> compileStm ( NamespaceStatement ns ) =
>     '\n' : compileNamespace ns ++ "\n"
> compileStm ( RequireOnceStatement file ) =
>     "require_once '" ++ file ++ "';\n\n"
> compileStm ( PhpClass name inherits statements ) =
>     concat [ compileClassSignature name inherits,
>              "{\n",
>              indent ( compileClassStms statements ),
>              "}\n" ]

Compiling namespaces is easy... just sandwich them with "namespace "
and ";".

> compileNamespace :: String -> String
> compileNamespace = ( "namespace "++ ) . ( ++";\n" )


Compiling classes
-----------------

The signature of a class is the "class X implements Y" part of the
class.

> compileClassSignature :: ClassName -> Inheritance -> String

Stick a newline on the result of compiling the inheritance, and then
stick the class at the start of it (with a space)

> compileClassSignature cls =
>     ( sandwichClassName cls ++ ) . ( ++ "\n" ) . compileInheritance
>     where
>     sandwichClassName = ( "class " ++ ) . ( ++ " " )
>     compileInheritance ( Extends a )      = "extends " ++ a
>     compileInheritance ( Implements a )   = "implements " ++ a
>     compileInheritance ( DoesNotInherit ) = ""

***

This function compiles the list of statements making up a class body
into a string.  It doesn't need to handle indenting all the class
statements inward one level-- that's done upstream.

> compileClassStms :: [ PhpClassStatement ] -> String
> compileClassStms = remTrailingNewlines . concatMap compileClassStm
>     where
>     compileClassStm ( PhpClassComment comment ) =
>         compileComment comment
>     compileClassStm ( PhpClassMethod method ) =
>         compileMethod method ++ "\n\n"
>     compileClassStm ( PhpClassField field ) =
>         compileField field ++ "\n\n"
>     -- The following is probably woefully inefficient...
>     remTrailingNewlines = reverse . remInitNewlines . reverse
>     remInitNewlines [] = []
>     remInitNewlines ( '\n':xs ) = remInitNewlines xs
>     remInitNewlines str = str


Compiling a field
-----------------

Compiling a field statement is quite simple, compared to some other
parts of our PHP subset.

> compileField :: PhpField -> String
> compileField ( Const name definition ) =
>     concat [ "const ", name, " = ", definition, ";\n" ]
> compileField ( StaticVar vis name maybeDef ) =
>     "static " ++ compileField ( Var vis name maybeDef )
> compileField ( Var vis name maybeDef ) =
>     concat [ compileVisibility vis,
>              " $",  name,
>              defTail,
>              ";\n" ]
>         where
>         defTail = maybe "" ( ( " = "++ ) . compileExpr) maybeDef