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

This module takes an abstract representation of a PHPDoc comment, and
produces from it a concrete PHP comment block, adequately formatted to
make PHP CodeSniffer using PEAR conventions happy.

> module Rapier.Obgen.Php.CompileComment
>     ( compileComment
>       -- :: PhpComment -> String
>     ) where
> import Rapier.Obgen.Php.CompileCommon
> import Rapier.Obgen.Php.Types
> import Data.List
> import Data.Maybe


Docblock items to @-prefixes
----------------------------

Following is a little pattern-matching exercise converting datatypes
representing docblock items to the strings containing their prefixes.
For example, DcAuthor maps to @author.

A little white lie follows:- DcParamList is mapped to "", though in
reality the items within a param list have the @-prefix "param".  This
is because the param block is a law unto itself and doesn't justify
with the rest of the comments.

> prefixOf :: DocCommentItem -> String
> prefixOf ( DcParamList _ ) = ""
> prefixOf ( DcReturn _ _ )  = "@return"
> prefixOf ( DcCategory _ )  = "@category"
> prefixOf ( DcPackage _ )   = "@package"
> prefixOf ( DcLink _ )      = "@link"
> prefixOf ( DcAuthor _ _ )  = "@author"
> prefixOf ( DcLicence _ _ ) = "@license" -- Woo, American English


Compiling comments
------------------

Now for compiling comments.  This is mostly trivial, _except_ for the
case of compiling a docblock.

> compileComment :: PhpComment -> String
> compileComment ( LineComment str ) = concat [ "// ", str, "\n" ]
> compileComment ( BlockComment str ) = concat [ "/* ", str, " */" ]

To compile a doc comment, we:

1. Make up a list of the items to go into the doc comment (the comment
   brief, the comment details or "" if they're missing, and the result
   of compiling all the doc-comment @-items);

2. Merge all those items, ousting any empty strings and separating the
   remainder with double newlines;

3. Flush the resulting comment body with asterisks for formatting;

4. Sandwich the asterisked body with the doc comment opening and
   closing syntax.

> compileComment ( DocComment brief maybeDetails items ) =
>     concat [ "/**\n",
>              addAsterisksTo commentBody,
>              " */\n" ]
>     where
>     addAsterisksTo = unlines . map ( " * "++ ) . lines
>     commentBody =
>         mergeWithSeparators [ brief,
>                               details,
>                               compiledItems ]
>             where
>             details = fromMaybe "" maybeDetails
>             mergeWithSeparators =
>                 intercalate "\n\n" . dumpEmptyStrings
>             compiledItems = compileItems items
>             dumpEmptyStrings = ( \\[ "" ] )

Compiling all the items itself requires breaking down a little:

1. Find out what the longest @-prefix is (so we can indent all the
   comment items such that the @-prefixes are all in one column).
   This is done by getPrefixLength, which returns the tuple of the
   longest prefix and the inputted list of items.

2. Compile all the items, passing in the length of the longest prefix;

3. Merge them all with double newlines separating each one.

The result is a string that can be piped into the comment body
compiler.

> compileItems :: [ DocCommentItem ] -> String
> compileItems items =
>     ( intercalate "\n\n" . map ( compileDocItem longestPrefix ) )
>     items
>         where
>         longestPrefix = maximum ( map ( length . prefixOf ) items )


***

> compileDocItem :: Int -> DocCommentItem -> String

Compiling param lists is comparatively very hard, because to comply
with certain coding standards we need to line up all the parameter
types and names.  This involves quite a lot of footwork on our part,
so we'll fan it out into a separate function for now.  Incidentally,
it needs to be a special case here (using the general pattern match
introduced later would cause only one @param to be printed...!)

> compileDocItem _ ( DcParamList params ) =
>     compileDocParamList params

> compileDocItem maxPadding item =
>     concat [ prefix,
>              makeFiller ( maxPadding - length prefix + 1 ),
>              cpInner item
>            ]
>         where
>         prefix = prefixOf item
>         cpInner ( DcParamList _ ) =
>             error "This should have been caught as a special case."
>         cpInner ( DcCategory cat ) = cat
>         cpInner ( DcLink link ) = link
>         cpInner ( DcPackage ns ) = ns
>         cpInner ( DcReturn rType comment ) =
>             concat [ typeName rType, " ", comment ]
>         cpInner ( DcLicence uri description ) =
>             concat [ uri, " ", description ]
>         cpInner ( DcAuthor author email )
>             | email == [] = author
>             | otherwise   = concat [ author, " <", email, ">" ]

***

> compileDocParamList :: [ DcParam ] -> String
> compileDocParamList params =
>     ( intercalate "\n" .
>       map ( addPrefix . compileDocParam maxTypeLen maxNameLen )
>     ) params
>     where
>     addPrefix = ( "@param " ++ )
>     ( maxTypeLen, maxNameLen ) =
>         foldr getMaxLengths ( -1, -1 ) params

***

Given a current estimate of the maximum type and name lengths and a
parameter, returns the result of updating the estimate to reflect the
lengths of the just-seen parameters.

This is used in a fold above to get the maximum lengths.

typeName is introduced in compileCommon.

> getMaxLengths :: DcParam -> ( Int, Int ) -> ( Int, Int )
> getMaxLengths ( CommentedParam t n _ ) ( a, b ) =
>     ( max a a', max b b' )
>     where
>     a' = length ( typeName t )
>     b' = length n

***

Given the maximum length of the type name column, the parameter name
column, the current output of the param list compiler and the next
param to compile, returns the output appended to with the compiled
form of the given param docstring.

The maximum lengths are used to force the type names, parameter names,
and comments to line up into columns.  It's a somewhat convoluted
operation, but it keeps PHP CodeSniffer happy and lets us show off a
bit.

> compileDocParam :: Int -> Int -> DcParam -> String
> compileDocParam maxTypeLen maxNameLen ( CommentedParam t n c ) =
>     concat [ typeName t, tFiller, n, nFiller, c ]
>         where
>         tFiller = makeFiller ( maxTypeLen - length ( typeName t )
>                                + 1 )
>         nFiller = makeFiller ( maxNameLen - length n
>                                + 1 )

***

This tiny function just spits out a space the given number of times!

> makeFiller :: Int -> String
> makeFiller = flip replicate ' '

