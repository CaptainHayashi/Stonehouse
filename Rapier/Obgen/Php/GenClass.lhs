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

This module is responsible for converting an abstract Rapier object
into a PHP class.

> module Rapier.Obgen.Php.GenClass
>     ( genClass
>       -- :: Namespace -> ClassName -> Metadata -> [ ObjectFieldDef ]
>       --    -> [ SpecialField ] -> [ PhpStatement ]
>     ) where
> import Rapier.Obgen.Object
> import Rapier.Obgen.Php.Types
> import Rapier.Obgen.Php.Namespaces ( qualifyPhpClassName )
> import Rapier.Obgen.Php.GenComment
> import Rapier.Obgen.Php.GenArrayConverters ( genToArray,
>                                              genFromArray )
> import Rapier.Obgen.Php.Utils ( toPhpFieldName,
>                                 toPhpParamName,
>                                 toPhpIndexName,
>                                 toPhpAccessorName,
>                                 rapierTypeToPhp,
>                                 fieldAccess,
>                                 fieldKeyFor
>                               )
> import Rapier.Obgen.Utils ( lowerCaseInitial,
>                             enquote
>                           )


Constants
---------

This is the inheritance node that is tacked on to generated PHP
classes.

> genericInheritance :: Inheritance
> genericInheritance = Implements ( qualifyPhpClassName "Object" )


Useful function: Deriving the name of a field
---------------------------------------------

> fieldName :: ObjectField -> String
> fieldName ( name :- _ ) = name


Another useful function: Deriving the field of a field def
----------------------------------------------------------

> fieldOfDef :: ObjectFieldDef -> ObjectField
> fieldOfDef ( Uncommented f ) = f
> fieldOfDef ( _ :>> f ) = f


The above two functions compose....

> fieldNameOfDef :: ObjectFieldDef -> String
> fieldNameOfDef = fieldName . fieldOfDef


Class generator
---------------

Given the PHP namespace, PHP name, metadata and fields of a Rapier
object, we can use genClass to generate the actual class statements.

Many of the different parts of the class generator don't need to know
about any comments attached to a field or its Rapier type, so we just
pass in the field name (fname) to make things tidier.

> genClass :: Namespace -> ClassName -> Metadata -> [ ObjectFieldDef ]
>          -> [ SpecialField ] -> [ PhpStatement ]
> genClass ns cn metadata defs sfs = [ docblock, classdef ]
>     where
>     docblock =
>         CommentStatement ( makeClassComment ns metadata )
>     classdef = PhpClass cn genericInheritance stms
>         where
>         stms = concat [ makeFieldDeclarations defs,
>                         makeConstants sfs fields,
>                         makeConstructor defs,
>                         makeFieldAccessorStms defs,
>                         genToArray fnames,
>                         genFromArray cn fnames,
>                         metadataRetriever
>                       ]
>         fnames = map fieldNameOfDef defs
>         fields = map fieldOfDef defs

Following is the set of functions that make the class definition
statements.


1. Field declarations
---------------------

The first thing we do is make field declarations for every field in
the object specification.

> makeFieldDeclarations :: [ ObjectFieldDef ] -> [ PhpClassStatement ]
> makeFieldDeclarations = concatMap makeFieldDeclaration
>     where
>     makeFieldDeclaration ( comment :>> field ) =
>         PhpClassComment ( LineComment comment ) :
>         makeFieldDeclaration ( Uncommented field )
>     makeFieldDeclaration ( Uncommented field ) =
>         [ PhpClassField
>           ( Var
>             Private
>             ( ( toPhpFieldName . fieldName ) field )
>             Nothing
>           )
>         ]


2. Constants
------------

Secondly, we set up some constants, to allow the various different
names and aliases of each field to be accessed.  The series of
constants is as follows:

1. For each field, we set up a constant named FIELD_fname, where fname
   is the CAPITALISED_UNDERSCORED version of the Rapier field name,
   and set it to array ID (lowercase concatenated) version of the name.

2. We then make a pseudo-constant called NAME_ALIASES which stores an
   array, indexed by FIELD_fname, which stores the RAPIER_NAME
   (original) and ACCESSOR (camelCased, such that calling the method
   with this name will get the field value) names.

3. Finally, an SPECIAL_FIELDS array pseudo-constant contains mappings
   between Rapier special field aliases (such as "ID" and "Owner") to
   the relevant FIELD_xyz constants.

> makeConstants :: [ SpecialField ] -> [ ObjectField ]
>               -> [ PhpClassStatement ]
> makeConstants aliases fields =
>     concat [ classLineComment "ARRAY INDICES //" :
>              makeFieldNameConstants fnames,
>              classLineComment "FIELD NAME CONSTANTS //" :
>              makeFieldMetadataArray fields,
>              classLineComment "SPECIAL FIELDS //" :
>              makeSpecialFieldsArray aliases
>            ]
>     where fnames = map fieldName fields

***

Constants part 1: make field name constants.

> makeFieldNameConstants :: [ Identifier ] -> [ PhpClassStatement ]
> makeFieldNameConstants = concatMap makeFieldNameConstant
>     where
>     makeFieldNameConstant fname =
>         [ classLineComment
>           ( "Array index for " ++ fname ),
>           PhpClassField
>           ( Const
>             ( fieldKeyFor fname )
>             ( ( enquote . toPhpIndexName ) fname )
>           )
>         ]

***

Part 2: FIELD_METADATA.

> makeFieldMetadataArray :: [ ObjectField ] -> [ PhpClassStatement ]
> makeFieldMetadataArray fields =
>     [ PhpClassField
>       ( StaticVar Public "FIELD_METADATA"
>         ( Just ( ArrayExpr metadataArrays ) )
>       )
>     ]
>     where
>     metadataArrays = map makeMetadataSubArray fields

> makeMetadataSubArray :: ObjectField -> ArrayItem
> makeMetadataSubArray ( fname :- ftype ) =
>     StaticAccess "self" ( fieldKeyFor fname ) :=>:
>     ArrayExpr
>       [ IdExpr "\\URY\\API\\Helpers\\RAPIER_NAME" :=>:
>         SingleQuotedString fname,
>         IdExpr "\\URY\\API\\Helpers\\ACCESSOR" :=>:
>         SingleQuotedString ( toPhpAccessorName fname ),
>         IdExpr "\\URY\\API\\Helpers\\TYPE_ID" :=>:
>         SingleQuotedString ( show ftype )
>       ]

***

Part 3: SPECIAL_FIELDS.

> makeSpecialFieldsArray :: [ SpecialField ] -> [ PhpClassStatement ]
> makeSpecialFieldsArray specials =
>     [ PhpClassField
>       ( StaticVar Public "SPECIAL_FIELDS"
>         ( Just ( ArrayExpr specialFieldDefs ) )
>       )
>     ]
>         where
>         specialFieldDefs = map makeSpecialFieldDef specials

> makeSpecialFieldDef :: SpecialField -> ArrayItem
> makeSpecialFieldDef ( alias :<->: fname ) =
>     SingleQuotedString alias :=>:
>     StaticAccess "self" ( fieldKeyFor fname )



3. Constructor
--------------

Thirdly, each class needs a constructor, that takes in the values of
each field and populates the class with them.

Most of the work of the constructor is farmed out to a pre-written PHP
"filter function" which checks the consistency of the incoming
parameters, throws an exception if anything looks shady, and returns a
massaged version of the input otherwise.

> makeConstructor :: [ ObjectFieldDef ] -> [ PhpClassStatement ]
> makeConstructor defs = [ comment, PhpClassMethod method ]
>     where
>     comment = PhpClassComment ( makeConstructorComment defs )
>     method = Method Public "__construct" params stms
>         where
>         params = map fieldToParam fields
>             where
>             fieldToParam ( name :- rtype ) =
>                 rapierTypeToPhp rtype :$ toPhpParamName name
>         stms = map fieldToStm fields
>             where
>             fieldToStm ( name :- rtype ) =
>                 fieldAccess "this" ( toPhpFieldName name ) :=
>                 FunctionCallExpr
>                 "\\URY\\API\\Helpers\\filter_object_field"
>                 [ IdExpr
>                   ( ( enquote . show ) rtype ),
>                   IdExpr
>                   ( '$' : toPhpParamName name )
>                 ]
>         fields = map fieldOfDef defs


4. Field accessors
------------------

The fourth part of a class is the field accessor set, which describes
the methods that can be used to get the value of a field.

> makeFieldAccessorStms :: [ ObjectFieldDef ] -> [ PhpClassStatement ]
> makeFieldAccessorStms = concatMap gen
>     where
>     -- If no comment, synthesise one from the field name.
>     gen ( Uncommented field ) = gen ( fieldName field :>> field )
>     -- Base case.
>     gen ( comment :>> field ) =
>         [ accComment field, accMethod field ]
>             where
>             accComment ( _ :- rpType ) =
>                 PhpClassComment
>                 ( DocComment
>                   ( "Gets " ++ lowerCaseInitial comment )
>                   Nothing
>                   [ DcReturn ( rapierTypeToPhp rpType ) comment ]
>                 )
>             accMethod ( idName :- _ ) =
>                 PhpClassMethod
>                 ( Method Public (toPhpAccessorName idName) []
>                   [ Return
>                     ( IdExpr
>                       ( fieldAccess "this"
>                         ( toPhpFieldName idName )
>                       )
>                     )
>                   ]
>                 )


5. To-array converter
---------------------

This is the first part of the GenArrayConverters module, and is thus
described there.


6. From-array converter
-----------------------

This is the second part of the GenArrayConverters module, and is thus
described there.


7. Metadata retriever
---------------------

The metadata constants defined in part 2 are exposed as a method
getMetadata, which returns a Rapier object (of class "!Metadata")
containing the values of the constants.  This is Rapier's standard
reflection mechanism.

The metadata retrieving code is the same throughout all objects, so
the function returning it is a thunk.

> metadataRetriever :: [ PhpClassStatement ]
> metadataRetriever = [ comment, method ]
>     where
>     comment =
>         PhpClassComment
>         ( DocComment
>           "Returns the metadata object for this Rapier class."
>           Nothing
>           [ DcReturn (PObject className) "the metadata object" ]
>         )
>     method =
>         PhpClassMethod
>         ( StaticMethod Public "metadata" []
>           [ Return
>             ( New className
>               [ StaticAccess "self" "$FIELD_METADATA",
>                 StaticAccess "self" "$SPECIAL_FIELDS"
>               ]
>             )
>           ]
>         )
>     className = "\\URY\\API\\Metadata"


Convenience functions
---------------------

This function quickly makes a class statement wrapping up a line
comment.

> classLineComment :: String -> PhpClassStatement
> classLineComment = PhpClassComment . LineComment