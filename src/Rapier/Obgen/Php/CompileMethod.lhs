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

This module is responsible for compiling abstract representations of
PHP class methods to concrete PHP.

It is separate from the main Compile module simply because the
compilation of methods is one of the most involved parts of the
compiler.

> module Rapier.Obgen.Php.CompileMethod
>     ( compileMethod
>       -- :: PhpMethod -> String
>     ) where
> import Rapier.Obgen.Php.CompileCommon ( compileVisibility,
>                                         indent,
>                                         compileParams )
> import Rapier.Obgen.Php.CompileExpr ( compileExpr )
> import Rapier.Obgen.Php.Types


Compiling methods
-----------------

> compileMethod :: PhpMethod -> String
> compileMethod (StaticMethod vis name params body) =
>     "static " ++ compileMethod (Method vis name params body)
> compileMethod (Method vis name params body) =
>     concat [ compileVisibility vis,
>              " function ",
>              name,
>              compileMethodParams params,
>              maybeAddExtraNewline params,
>              "{\n",
>              indent (compileMethodStms body),
>              "}\n" ]
>     where
>     -- We want another newline between the param list and the
>     -- block, if there are <= 2 params (and thus the param list is
>     -- inline instead of being split over a few lines).
>     maybeAddExtraNewline []       = "\n"
>     maybeAddExtraNewline ( _:[] ) = "\n"
>     maybeAddExtraNewline _        = " " -- Add a space instead


***

This function compiles a list of method/function parameters using the
generic compileParams function (found in CompileCommon).

> compileMethodParams :: [ Param ] -> String
> compileMethodParams = compileParams compileMethodParam

***

The next function actually compiles a method parameter.  It makes
usage of PHP's parameter type checks where possible (at time of
writing, not much)

> compileMethodParam :: Param -> String
> compileMethodParam (phpType :$ name)
>     | phpType == PArray = "array $" ++ name
>     | otherwise         = '$':name

***

And this function compiles the body of the method (its set of
statements).

> compileMethodStms :: [PhpMethodStatement] -> String
> compileMethodStms = concatMap compileMethodStm
>     where
>     compileMethodStm ( NakedExpr expr ) = compileExpr expr ++ ";\n"
>     compileMethodStm ( ident := expr ) =
>         ident ++ " = " ++ compileExpr expr ++ ";\n"
>     compileMethodStm ( Return expr ) =
>         "return " ++ compileExpr expr ++ ";\n"
>     compileMethodStm _ = "// n0mits no\n"