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
> import Rapier.Obgen.Php.Constants
>     ( filterToTypestringFunction
>     , metadataClass
>     , metadataRapierNameIndex
>     , metadataAccessorIndex
>     , metadataTypeIdIndex
>     )
> import Language.Php.Syntax
> import Rapier.Obgen.Php.Namespaces
>     ( qualifyPhpClassName
>     )
> import Rapier.Obgen.Php.GenComment
> import Rapier.Obgen.Php.GenArrayConverters
>     ( genToArray
>     , genFromArray
>     )
> import Rapier.Obgen.Php.Utils
>     ( toPhpFieldName
>     , toPhpParamName
>     , toPhpIndexName
>     , toPhpAccessorName
>     , rapierTypeToPhp
>     , fieldAccess
>     , fieldKeyFor
>     )
> import Rapier.Obgen.Object
>     ( SpecialField
>     , Metadata       ( .. )
>     , ObjectFieldDef ( .. )
>     , ObjectField    ( .. )
>     , fieldName
>     , fieldOfDef
>     , fieldNameOfDef
>     , resolveDef
>     )
> import Rapier.Obgen.Utils
>     ( lowerCaseInitial
>     , enquote
>     )
> import Rapier.Utils
>     ( escape
>     , applyAll
>     )


Constants
---------

This is the inheritance node that is tacked on to generated PHP
classes.

> genericInheritance :: Inheritance
> genericInheritance =
>     Inheritance Nothing [ qualifyPhpClassName "Object" ]


Class generator
---------------

Given the PHP namespace, PHP name, metadata and fields of a Rapier
object, we can use genClass to generate the actual class statements.

Many of the different parts of the class generator don't need to know
about any comments attached to a field or its Rapier type, so we just
pass in the field name (fname) to make things tidier.

> genClass :: Namespace -> ClassName -> Metadata -> [ ObjectFieldDef ]
>          -> [ SpecialField ] -> [ TopStatement ]
> genClass ns cn metadata defs sfs = [ docblock, classdef ]
>     where
>     docblock =
>         TopComment ( makeClassComment ns metadata )
>     classdef = Class sig stms
>         where
>         sig = ClassSignature Public Concrete cn genericInheritance
>         stms = concat [ makeFieldDeclarations defs
>                       , makeConstants sfs fields
>                       , makeConstructor defs
>                       , makeFieldAccessorStms defs
>                       , genToArray fnames
>                       , genFromArray cn fnames
>                       , metadataRetriever
>                       ]
>         fnames = map fieldNameOfDef defs
>         fields = map fieldOfDef defs

Following is the set of functions that make the class definition
statements.


1. Field declarations
---------------------

The first thing we do is make field declarations for every field in
the object specification.

> makeFieldDeclarations :: [ ObjectFieldDef ] -> [ Member ]
> makeFieldDeclarations = concatMap makeFieldDeclaration
>     where
>     makeFieldDeclaration ( comment :>> field ) =
>         ClassComment ( LineComment comment ) :
>         makeFieldDeclaration ( Uncommented field )
>     makeFieldDeclaration ( Uncommented field ) =
>         [ ClassField
>           ( Var Instance
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
>               -> [ Member ]
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

> makeFieldNameConstants :: [ Identifier ] -> [ Member ]
> makeFieldNameConstants = concatMap makeFieldNameConstant
>     where
>     makeFieldNameConstant = applyAll [ makeComment, makeStm ]
>     makeComment = classLineComment . ( "Array index for " ++  )
>     makeStm fname = ClassField
>                     ( Const
>                       ( fieldKeyFor fname )
>                       ( ( enquote . toPhpIndexName ) fname )
>                     )

***

Part 2: FIELD_METADATA.

> makeFieldMetadataArray :: [ ObjectField ] -> [ Member ]
> makeFieldMetadataArray fields =
>     [ ClassField
>       ( Var Static Public "FIELD_METADATA"
>         ( Just ( ArrayExpr metadataArrays ) )
>       )
>     ]
>     where
>     metadataArrays = map makeMetadataSubArray fields

> makeMetadataSubArray :: ObjectField -> ArrayItem
> makeMetadataSubArray ( fname :- ftype ) =
>     StaticAccess "self" ( fieldKeyFor fname ) :=>:
>     ArrayExpr
>       [ metadataRapierNameIndex :=>:
>         SingleQuotedString ( escape '\'' fname )
>       , metadataAccessorIndex :=>:
>         SingleQuotedString ( toPhpAccessorName fname )
>       , metadataTypeIdIndex :=>:
>         SingleQuotedString ( escape '\'' ( show ftype ) )
>       ]

***

Part 3: SPECIAL_FIELDS.

> makeSpecialFieldsArray :: [ SpecialField ] -> [ Member ]
> makeSpecialFieldsArray specials =
>     [ ClassField
>       ( Var Static Public "SPECIAL_FIELDS"
>         ( Just ( ArrayExpr specialFieldDefs ) )
>       )
>     ]
>     where
>     specialFieldDefs = map makeSpecialFieldDef specials

> makeSpecialFieldDef :: SpecialField -> ArrayItem
> makeSpecialFieldDef ( alias, fname ) =
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

> makeConstructor :: [ ObjectFieldDef ] -> [ Member ]
> makeConstructor = applyAll [ genComment
>                            , ( genMeth . map fieldOfDef )
>                            ]
>     where
>     genComment = ClassComment . makeConstructorComment
>     genMeth fields = publicInstMethod "__construct" params stms
>         where
>         params = map fieldToParam fields
>             where
>             fieldToParam ( name :- rtype ) =
>                 rapierTypeToPhp rtype :$ toPhpParamName name
>         stms = map fieldToStm fields
>             where
>             fieldToStm ( name :- rtype ) =
>                 fieldAccess "this" ( toPhpFieldName name ) :=
>                 filterToTypestringFunction
>                 [ IdExpr
>                   ( '$' : toPhpParamName name ),
>                   IdExpr
>                   ( ( enquote . escape '\'' . show ) rtype )
>                 ]


4. Field accessors
------------------

The fourth part of a class is the field accessor set, which describes
the methods that can be used to get the value of a field.

> makeFieldAccessorStms :: [ ObjectFieldDef ] -> [ Member ]
> makeFieldAccessorStms =
>     concatMap ( applyAll [ genDoc, genMeth ] . resolveDef )
>     where
>     genDoc ( comment, _ :- rtype ) =
>         classDocComment ( "Gets " ++ lowerCaseInitial comment )
>                         Nothing
>                         [ DcReturn ( rapierTypeToPhp rtype )
>                                    comment
>                         ]
>     genMeth ( _, name :- _ ) = publicInstMethod
>                                ( toPhpAccessorName name )
>                                []
>                                [ Return
>                                  ( IdExpr
>                                    ( fieldAccess "this"
>                                      ( toPhpFieldName name )
>                                    )
>                                  )
>                                ]


5/6. Array converters
---------------------

These are in the GenArrayConverters module.


7. Metadata retriever
---------------------

The metadata constants defined in part 2 are exposed as a method
getMetadata, which returns a Rapier object (of class "!Metadata")
containing the values of the constants.  This is Rapier's standard
reflection mechanism.

The metadata retrieving code is the same throughout all objects, so
the function returning it is a thunk.

> metadataRetriever :: [ Member ]
> metadataRetriever = [ doc, meth ]
>     where
>     doc = classDocComment
>           "Returns the metadata object for this Rapier class."
>           Nothing
>           [ DcReturn ( PObject metadataClass )
>             "the metadata object"
>           ]
>     meth = publicStaticMethod "metadata" []
>            [ Return
>              ( New metadataClass
>                [ StaticAccess "self" "$FIELD_METADATA"
>                , StaticAccess "self" "$SPECIAL_FIELDS"
>                ]
>              )
>            ]
