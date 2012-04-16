Rapier.Obgen.Php.GenComment
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

This module provides functions to generate PHP documentation comments.

> module Rapier.Obgen.Php.GenComment
>     ( makeClassComment,
>       -- :: Namespace -> Metadata -> PhpComment
>       makeInitialComment,
>       -- :: Namespace -> ClassName -> Metadata -> PhpComment
>       makeConstructorComment,
>       -- :: [ ObjectFieldDef ] -> DocComment
>       toArrayComment,
>       -- :: DocComment
>       makeFromArrayComment
>       -- :: ClassName -> DocComment
>     ) where
> import Rapier.Obgen.Php.Types
> import Rapier.Obgen.Php.Utils
>     ( rapierTypeToPhp
>     , toPhpParamName
>     )
> import Rapier.Obgen.Object


Initial comment details
-----------------------

The initial comment contains the following details section.

> initialCommentDetails :: String
> initialCommentDetails
>     = ( init . unlines ) -- Removes trailing newline
>       [ "Part of Rapier"
>       , ""
>       , "PHP Version 5.4"
>       , ""
>       , "WARNING: This class has been automatically generated."
>       , "As such, manual changes to this class file may be lost"
>       , "if and when the class is re-generated."
>       ]


Generic metadata
----------------

The document comment for the PHP class depends on the metadata
provided with it. When an object has no valid metadata, this default
metadata is substituted in.

> genericMetadata :: Metadata
> genericMetadata = FullMetadata
>                   ( AuthorWithEmail
>                     "URY Computing Team"
>                     "computing@ury.york.ac.uk"
>                   )
>                   ( DescribedUriLicence
>                     "http://www.example.com"
>                     "Unknown"
>                   )
>                   "http://ury.york.ac.uk"
>                   "A Rapier object."
>                   Nothing


Class comment
-------------

Given object metadata, the following function creates a before-class
comment containing the metadata.

> makeClassComment :: Namespace -> Metadata -> Comment

If there is no metadata given, replace the (lack of) metadata with the
generic metadata and try again.

> makeClassComment ns ( NoMetadata ) =
>     makeClassComment ns genericMetadata

If there is a full set of metadata given, then we can make a comment
out of it.

> makeClassComment ns ( FullMetadata auth licence uri brief dets ) =
>     DocComment
>     brief
>     dets
>     [ DcCategory "Rapier"
>     , DcPackage ns
>     , DcAuthor (nameOf auth) (emailOf auth)
>     , makePhpLicence licence
>     , DcLink uri
>     ]
>     where nameOf  ( AuthorWithEmail name _  ) = name
>           nameOf  ( AuthorNameOnly  name    ) = name
>           emailOf ( AuthorWithEmail _ email ) = email
>           emailOf ( AuthorNameOnly  _       ) = ""


Initial comment
---------------

Given an object's PHP namespace, PHP name and metadata, creates a
top-of-file comment.

Because the result of this function is so similar to that of the class
docblock generator (makeClassComment), it is actually defined in
terms of it.  We "patch" the metadata to replace its brief with the
class name and details with a generic statement about which versions
of PHP are supported etc.

> makeInitialComment :: Namespace -> ClassName -> Metadata -> Comment
> makeInitialComment ns cn = makeClassComment ns . patch
>         where
>         patch ( NoMetadata ) = patch genericMetadata
>         patch ( FullMetadata author licence uri _ _ ) =
>             FullMetadata author licence uri brief' details'
>                 where brief'   = concat [ ns, "\\", cn, " class" ]
>                       details' = Just initialCommentDetails


Constructor comment
-------------------

Given the set of fields in an object, this function creates a comment
to go with the object's constructor.

> makeConstructorComment :: [ ObjectFieldDef ] -> Comment
> makeConstructorComment fields =
>     DocComment
>     "Constructs the object."
>     Nothing
>     [ DcParamList ( map makeParam fields ) ]
>     where
>     makeParam ( comment :>> field ) =
>         makeParamFromField field comment
>     makeParam ( Uncommented field ) =
>         makeParamFromField field "An unknown element."


To-array comment
----------------

This is a canned to-array comment.

> toArrayComment :: [ Identifier ] -> Comment
> toArrayComment =
>     const ( DocComment
>             "Converts the object to its array form."
>             Nothing
>             [ DcReturn PArray "the object, in array form" ]
>           )


From-array comment
------------------

This is a (mostly canned, with the exception of a customisable
class-name) from-array comment.

> makeFromArrayComment :: ClassName -> Comment
> makeFromArrayComment cn =
>     DocComment
>     "Converts an object of this type from its array form."
>     Nothing
>     [ DcParamList
>       [ CommentedParam
>         PArray "$array" "the object, in array form"
>       ],
>       DcReturn ( PObject cn ) "the converted object"
>     ]


Generating a licence docstring
------------------------------

Given a licence term, this tiny function generates the appropriate PHP
docstring for it.

> makePhpLicence :: Licence -> DocCommentItem
> makePhpLicence ( DescribedUriLicence uri details ) =
>     DcLicence uri details


Generating an @param from a field
---------------------------------

Given a Rapier field and its comment, this function generates a
DcParam stanza for it.

This is mainly used to make the @param set for the object constructor,
in which every incoming parameter is a value for one of the object's
fields.

> makeParamFromField :: ObjectField -> String -> DcParam
> makeParamFromField ( name :- rtype ) =
>     CommentedParam ptype name'
>         where
>           ptype = rapierTypeToPhp rtype
>           name' = '$' : toPhpParamName name