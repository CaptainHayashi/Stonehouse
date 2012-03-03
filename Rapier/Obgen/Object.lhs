Rapier.Obgen.Object
===================

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

This module contains the algebraic data types that constitute a Rapier
/object specification/, which is the generic unit describing a Rapier
object that is used to make specific implementations of the object in
concrete languages.

As this is a data-type-only module, we expose everything to other
modules.

> module Rapier.Obgen.Object where

Type synonyms
-------------

An identifier is a kind of string.

> type RpIdentifier = String

As is a Rapier class (for now, anyway; further implementations might
generalise the Rapier class).

> type RapierClass = String


Rapier types
------------

Following is an enumeration of all the different Rapier types, two of
which (the vector and the map) are recursive.

> data RapierType = RpInteger
>                 | RpNatural
>                 | RpReal
>                 | RpDate
>                 | RpTime
>                 | RpString
>                 | RpBoolean
>                 | RpVector RapierType
>                 | RpMap RapierType
>                 | RpObject String
>                 | RpArray Int RapierType
>                 deriving (Show, Read)


Object fields
-------------

Rapier objects are made primarily up of named, optionally commented
fields.

An object field definition can be an uncommented field...

> data ObjectFieldDef = Uncommented ObjectField

Or a comment attached to a field.

>                     | String :>> ObjectField
>                       deriving (Show, Read)

An object field definition is thus...

> data ObjectField = RpIdentifier :- RapierType
>                  deriving (Show, Read)

> infixr 5 :>>
> infixr 6 :-

***

A licence tag is a coupling of a URL pointing to a licence followed by
a human-readable name for the licence.

> data Licence = DescribedUrlLicence String String
>              deriving (Show, Read)

***

Author, licence, brief, maybe details.

> data Metadata = FullMetadata Author Licence String String
>     ( Maybe String )
>               | NoMetadata
>                 deriving (Show, Read)

***

An author can either be a name combined with an email address, or just
a name.

> data Author = AuthorWithEmail String String
>             | AuthorNameOnly String
>               deriving (Show, Read)

***

A special field definition is a mapping from a string (the special
field alias) to another string (the actual field name).

> data SpecialField = String :<->: String
>                   deriving (Show, Read)

> infixr 5 :<->:

***

Name, metadata, fields

> data ObjectSpec
>     = ObjectSpec String Metadata [ObjectFieldDef] [SpecialField]
>       deriving (Show, Read)