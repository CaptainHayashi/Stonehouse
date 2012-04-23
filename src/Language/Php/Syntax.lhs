Rapier.Obgen.Php.Types
======================

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

This module collates the algebraic data types that abstractly define
the subset of PHP we use to implement Rapier objects.

> module Rapier.Obgen.Php.Types where

Type synonyms
-------------

A class name is just a string.

> type ClassName = String

As is a namespace.

> type Namespace = String

And an author name...

> type AuthorName = String

...and an email address...

> type Email = String

...and an identifier (variable name)

> type Identifier = String


Data types
----------

A PHP program is a list of statements.

> data Php = StatementList [Statement]
>          deriving ( Show, Read )

***

Inheritance in PHP is either "extends" (which implies inheriting from
a parent class) or "implements" (inheriting from an interface).

> data Inheritance = Extends String
>                  | Implements String
>                  | DoesNotInherit
>                  deriving ( Show, Read )

***

For the purposes of Obgen, an outside-of-class PHP statement is a
comment, a namespace declaration, an include or a class.

(Obviously, this doesn't capture the full breadth of the PHP language;
we're just implementing a safe subset that we can use to generate
objects.)

> data Statement = CommentStatement Comment
>                   | NamespaceStatement Namespace
>                   | RequireOnceStatement String
>                     -- Filename to be required
>                   | Class String Inheritance
>                     [ ClassStatement ]
>                     -- Name, inheritance and body
>                   deriving ( Show, Read )

***

Inside a class, Obgen expects methods and fields.

> data ClassStatement = ClassComment Comment
>                     | ClassMethod Method
>                     | ClassField Field
>                       deriving ( Show, Read )

The next set of functions create comments within a class.

> classLineComment :: String -> ClassStatement
> classLineComment = ClassComment . LineComment
> classBlockComment :: String -> ClassStatement
> classBlockComment = ClassComment . BlockComment
> classDocComment :: String -> Maybe String -> [ DocCommentItem ]
>                 -> ClassStatement
> classDocComment = ClassComment `compose3` DocComment
>     where compose3 = ( . ) . ( . ) . ( . )

***

A field is a constant or a variable, or a
visibility-qualified static variable.  Consts must be initialised to
an expression; vars do not (they will be initialised in the
constructor) and thus their initialiser is optional.

> data Field = Const Identifier String
>            | Var Scope Visibility Identifier ( Maybe Expr )
>              deriving ( Show, Read )

Methods
-------

A method is a static method or a normal method.  Both have a name, and
a set of statements inside them.

> data Method =
>     Method Scope Visibility String [ Param ] [ MethodStatement ]
>            deriving ( Show, Read )

Because methods are only found in class statements, the function we'll
provide to create one automatically wraps it up in a
ClassStatement.

> method :: Scope -> Visibility -> String -> [ Param ]
>        -> [ MethodStatement ] -> ClassStatement
> method = ClassMethod `compose5` Method
>     where compose5 = ( . ) . ( . ) . ( . ) . ( . ) . ( . )

Some shorthand for frequently used method types:

> publicInstMethod :: String -> [ Param ] -> [ MethodStatement ]
>                  -> ClassStatement
> publicInstMethod = method Instance Public

> publicStaticMethod :: String -> [ Param ] -> [ MethodStatement ]
>                    -> ClassStatement
> publicStaticMethod = method Static Public


***

A method parameter is an enjoinment of an identifier and a type.  (The
type is, at time of writing, usually dropped, but hopefully one day
PHP's parameter type checking will get better!)

> data Param = Type :$ Identifier
>              deriving ( Show, Read )

> infixr 5 :$

***

A method statement, as far as we're concerned for this subset of PHP,
can be a function call, a method call, or a comment.

> data MethodStatement = NakedExpr Expr
>                      | InMethodComment Comment
>                      | Identifier := Expr
>                      | Return Expr
>                        deriving ( Show, Read )

***

Let's define a (workable subset of) a PHP expression.

> data Expr = IdExpr Identifier
>           | StaticAccess Identifier Identifier
>           | IntLiteral Integer
>           | SingleQuotedString String
>           | DoubleQuotedString String
>           | FunctionCallExpr String [ Expr ]
>           | ArrayExpr [ ArrayItem ]
>           | New ClassName [ Expr ]
>           | ArraySubscript Identifier Expr
>             deriving ( Show, Read )

***

> data ArrayItem = ImplicitKey Expr
>                | Expr :=>: Expr -- LHS is key, RHS value
>                  deriving ( Show, Read )

> infixr 5 :=>:

***

Visibility is public, protected or private.

> data Visibility = Public | Protected | Private
>                   deriving ( Show, Read )


***

Scope is static or instance.

> data Scope = Static | Instance
>              deriving ( Show, Read )


These are organised into a list, so they can be properly formatted.

> data DocCommentItem = DcParamList [ DcParam ]

Return type and comment

>                     | DcReturn Type String
>                     | DcCategory String
>                     | DcPackage Namespace
>                     | DcLink String
>                     | DcAuthor AuthorName Email

URI and description of licence

>                     | DcLicence String String
>                     deriving ( Show, Read )

**

Type, name and comment

> data DcParam = CommentedParam Type String String
>              deriving ( Show, Read )

***

> data Comment = LineComment String
>              | BlockComment String
>              | DocComment String
>                ( Maybe String ) [ DocCommentItem ]
>                 deriving ( Show, Read )

PHP types
---------

Following is an enumeration of all the possible PHP types.

> data Type = PString
>           | PInteger
>           | PBool
>           | PCallback
>           | PDouble
>           | PMixed
>           | PArray
>           | PObject String
>             deriving ( Show, Read, Eq )
