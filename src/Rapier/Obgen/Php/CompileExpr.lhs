Rapier.Obgen.Php.CompileExpr
============================

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

This module is responsible for compiling PHP expressions into PHP
code.

> module Rapier.Obgen.Php.CompileExpr
>     ( compileExpr
>       -- :: PhpExpr -> String
>     ) where
> import Rapier.Obgen.Php.Types
> import Rapier.Obgen.Php.CompileCommon ( compileParams )
> import Rapier.Obgen.Utils ( enquote,
>                             doubleEnquote )


Compiling an expression
-----------------------

This function takes a PHP expression as an algebraic data expression
and compiles it into a string.

> compileExpr :: PhpExpr -> String
> compileExpr ( StaticAccess className ident ) =
>     className ++ "::" ++ ident;
> compileExpr ( IdExpr ident ) = ident;
> compileExpr ( FunctionCallExpr name args ) =
>     name ++ compileArguments args
> compileExpr ( ArrayExpr items ) = "array " ++ compileArrayBody items
> compileExpr ( ArraySubscript array subscript ) =
>     array ++ "[ " ++ compileExpr subscript ++ " ]"
> compileExpr ( SingleQuotedString string ) = enquote string
> compileExpr ( DoubleQuotedString string ) = doubleEnquote string
> compileExpr ( New name args ) =
>     "new " ++ name ++ compileArguments args
> compileExpr ( IntLiteral int ) = show int


Compiling function call arguments
---------------------------------

This function compiles a list of function call arguments, by applying
the generic CompileCommon compileParams function with each argument
being compiled as an expr.

> compileArguments :: [ PhpExpr ] -> String
> compileArguments = compileParams compileExpr


Compiling an array body
-----------------------

Another usage of compileParams is compiling the body of an array
literal.

> compileArrayBody :: [ ArrayItem ] -> String
> compileArrayBody = compileParams compileArrayItem
>     where
>     compileArrayItem (ImplicitKey value) = compileExpr value
>     compileArrayItem (key :=>: value) = ckey ++ " => " ++ cvalue
>         where
>           ckey = compileExpr key
>           cvalue = compileExpr value