Rapier.Obgen.Php.Constants
==========================

Part of Stonehouse, the Rapier Object Code Generator

As a set of purely trivial data, this file (Constants.lhs) is hereby
placed under the public domain where legally possible.

***

This module collates several constants or partial applications that
may need to be tweaked over the lifetime of Stonehouse's PHP
generator.

> module Rapier.Obgen.Php.Constants
>     ( mainNamespace              -- ShowS
>     , helpersNamespace           -- ShowS
>       -- External PHP functions
>     , filterToTypestringFunction -- [ Expr ] -> Expr
>     , failIfNotInArrayFunction   -- [ Expr ] -> Expr
>       -- Class names
>     , metadataClass
>       -- Field metadata indices
>     , metadataRapierNameIndex    -- Expr
>     , metadataAccessorIndex      -- Expr
>     , metadataTypeIdIndex        -- Expr
>     )
> where
> import Language.Php.Syntax
>     ( Expr ( FunctionCallExpr
>            , IdExpr
>            )
>     , Namespace ( AbsoluteRoot
>                 , ( :\ )
>                 )
>     )

Namespaces
----------

mainNamespace is the main namespace of PHP Rapier.

> mainNamespace :: Namespace
> mainNamespace = AbsoluteRoot :\ "URY" :\ "API"

helpersNamespace is where the set of Rapier helper functions is
rooted.

> helpersNamespace :: Namespace -> Namespace
> helpersNamespace = ( :\ "Helpers" )


Class names
-----------

metadataClass is the PHP class corresponding to the metadata object.

> metadataClass :: ClassName
> metadataClass = NamespacedClass mainNamespace "Metadata"


External PHP functions
----------------------

A convenience definition: helperFunction takes the name of a function
and returns a partial function call expression placing it into the
helpers namespace.

> helperFunction :: String -> [ Expr ] -> Expr
> helperFunction = FunctionCallExpr . helpersNamespace . ( "\\" ++ )

filterToTypestringFunction is the PHP function invoked when a variable
needs to be Rapier typechecked against a typestring.

> filterToTypestringFunction :: [ Expr ] -> Expr
> filterToTypestringFunction = helperFunction "filterToTypestring"

failIfNotInArrayFunction is the PHP function that throws an exception
if the second argument is not an array key in the first argument.

> failIfNotInArrayFunction :: [ Expr ] -> Expr
> failIfNotInArrayFunction = helperFunction "filterToTypestring"


Field metadata indices
----------------------

The indexMetadata.. constants are the array indices for the field
metadata array.

> metadataRapierNameIndex, metadataAccessorIndex, metadataTypeIdIndex
>     :: Expr
> metadataRapierNameIndex = IdExpr "RAPIER_NAME"
> metadataAccessorIndex   = IdExpr "ACCESSOR"
> metadataTypeIdIndex     = IdExpr "TYPE_ID"