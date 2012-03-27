Rapier.Obgen.Php.Generate
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

This module takes in a JSON document representing a Rapier object
specification, and converts it into an abstract Rapier object (see
Rapier.Obgen.Object).

> module Rapier.Obgen.Json.Source ( source ) where
> import Text.JSON
> import Data.Char ( isSpace )
> import Rapier.Obgen.Object
> import Rapier.Types ( RapierType )


JSON instances
--------------

Most of the work done in order to make Rapier objects readable from
JSON is spent in creating instances of the JSON typeclass for each
item in an object specification.

Each type involved in a Rapier object specification is shadowed by a
"J" type variant, which implements an extension of the JSON typeclass.
This is done to avoid orphaned instances and all the apparent fun they
can cause.

Firstly, some convenience functions.  The function toJSStrVal takes a
String and wraps it up in a JSValue containing a JSString (used
whenever we need to provide a string in ShowJSON):

> toJSStrVal :: String -> JSValue
> toJSStrVal = JSString . toJSString

Because many attempts to pull data from a JSON object during parsing
operate on Strings, a convenience function stringFromObj is defined as
a wrapper around valFromObj.

> stringFromObj :: String -> JSObject JSValue -> Result String
> stringFromObj = valFromObj

makeValueJS and its list mapping makeValuesJS are intended to convert
the values of key-value maps from Strings into JSValues.

> makeValuesJS :: [ ( a, String ) ] -> [ ( a, JSValue ) ]
> makeValuesJS = map makeValueJS

> makeValueJS :: ( a, String ) -> ( a, JSValue )
> makeValueJS ( k, v ) = ( k, toJSStrVal v )

Finally, the function defaultingTo takes a Result and a default value
x, and replaces it with Ok x if the result is an Error.

> defaultingTo :: Result a -> a -> Result a
> defaultingTo ( Ok a    ) _ = Ok a
> defaultingTo ( Error _ ) b = Ok b

***

And now, the instances.  Let's start with ObjectFieldDef:

> newtype JObjectFieldDef = JObjectFieldDef ObjectFieldDef

> fromJObjectFieldDef :: JObjectFieldDef -> ObjectFieldDef
> fromJObjectFieldDef ( JObjectFieldDef a ) = a

> instance JSON JObjectFieldDef where
>     showJSON ( JObjectFieldDef ( Uncommented x ) ) =
>         makeObj [ ( "name", nameOf x ),
>                   ( "type", typeOf x )
>                 ]
>     showJSON ( JObjectFieldDef ( comment :>> x ) ) =
>         makeObj [ ( "comment", toJSStrVal comment ),
>                   ( "name"   , nameOf x           ),
>                   ( "type"   , typeOf x           )
>                 ]
>     readJSON ( JSObject obj ) =
>         do name  <- stringFromObj "name" obj
>            rtype <- valFromObj    "type" obj :: Result JRapierType
>            return ( JObjectFieldDef
>                     ( def
>                       ( name :- fromJRapierType rtype )
>                       ( stringFromObj "comment" obj )
>                     )
>                   )
>                where
>                {- Since we don't mind whether or not we got a
>                comment, we have to write this next bit so that a
>                lack of comment doesn't ruin the entire parse. -}
>                def field ( Ok    comment ) = comment :>> field
>                def field ( Error _       ) = Uncommented field
>     readJSON _ = Error "Field definition must be an object."

Now to define some helpful functions used above:

> nameOf :: ObjectField -> JSValue
> nameOf ( name :- _     ) = toJSStrVal name

> typeOf :: ObjectField -> JSValue
> typeOf ( _    :- rtype ) = toJSStrVal ( show rtype )

***

ObjectFields never turn up on their own (ie without being inside an
ObjectFieldDef) inside a specification, so we don't provide a JSON
instance for them.

***

Licence next--this one is quite easy:

> newtype JLicence = JLicence Licence

> fromJLicence :: JLicence -> Licence
> fromJLicence ( JLicence a ) = a

> instance JSON JLicence where
>     showJSON ( JLicence ( DescribedUriLicence uri description ) ) =
>         makeObj [ ( "uri"        , toJSStrVal uri         ),
>                   ( "description", toJSStrVal description )
>                 ]
>     readJSON ( JSObject obj ) =
>         do uri         <- stringFromObj "uri"         obj
>            description <- stringFromObj "description" obj
>            return ( JLicence ( DescribedUriLicence uri
>                                                    description
>                              )
>                   )
>     readJSON _ = Error "Licence must be an object."

***

Author next.  An author with no email is just a string; an author with
an email is an object with the name and email as separate items.

> newtype JAuthor = JAuthor Author

> fromJAuthor :: JAuthor -> Author
> fromJAuthor ( JAuthor a ) = a

> instance JSON JAuthor where
>     showJSON ( JAuthor ( AuthorWithEmail name email ) ) =
>         makeObj [ ( "name" , toJSStrVal name  ),
>                   ( "email", toJSStrVal email )
>                 ]
>     showJSON ( JAuthor ( AuthorNameOnly name ) ) = toJSStrVal name
>     readJSON ( JSObject obj ) =
>         -- Object => AuthorWithEmail (see above)
>         do name  <- stringFromObj "name"  obj
>            email <- stringFromObj "email" obj
>            return ( JAuthor ( AuthorWithEmail name email ) )
>     readJSON ( JSString str ) =
>         Ok ( JAuthor ( AuthorNameOnly ( fromJSString str ) ) )
>     readJSON _ = Error "Author must be an object or a string."

***

Metadata next.  Note that though the JSON representation of NoMetadata
is given here as the string "none", in practice the absence of
metadata is specified higher up in ObjectSpec by omitting a "metadata"
key entirely.

> newtype JMetadata = JMetadata Metadata

> fromJMetadata :: JMetadata -> Metadata
> fromJMetadata ( JMetadata a ) = a

> instance JSON JMetadata where
>     showJSON ( JMetadata ( FullMetadata author
>                                         licence
>                                         uri
>                                         brief
>                                         maybeDetails
>                          )
>              ) = makeObj objMap
>         where
>         objMap = addDetailsIfPresent maybeDetails
>                  ++ [ ( "author" , showJSON ( JAuthor  author  ) ),
>                       ( "licence", showJSON ( JLicence licence ) ),
>                       ( "uri"    , showJSON uri     ),
>                       ( "brief"  , showJSON brief   )
>                     ]
>         addDetailsIfPresent ( Nothing      ) = []
>         addDetailsIfPresent ( Just details ) =
>             [ ( "details", toJSStrVal details ) ]
>     showJSON ( JMetadata NoMetadata ) = toJSStrVal "none"
>     readJSON ( JSObject obj ) =
>         do author  <- valFromObj    "author"  obj :: Result JAuthor
>            licence <- valFromObj    "licence" obj :: Result JLicence
>            uri     <- stringFromObj "uri"     obj
>            brief   <- stringFromObj "brief"   obj
>            return ( mkMetadata
>                     ( fromJAuthor  author  )
>                     ( fromJLicence licence )
>                     uri
>                     brief
>                     ( stringFromObj "details" obj )
>                   )
>            where
>            mkMetadata a l u b ( Ok details ) =
>                JMetadata ( FullMetadata a l u b ( Just details ) )
>            mkMetadata a l u b ( Error _    ) =
>                JMetadata ( FullMetadata a l u b Nothing          )
>     readJSON ( JSString str )
>         | fromJSString str == "none" = Ok ( JMetadata NoMetadata )
>         | otherwise =
>             Error "Only string allowed here is 'none'."
>     readJSON _ = Error "Metadata must be 'none' or an object."

***

SpecialField is just a tuple of strings, so we'll worry about how to
represent those when we get to ObjectSpec.

***

RapierType is mostly an exercise in using our instances for Show and
Read:

> newtype JRapierType = JRapierType RapierType

> fromJRapierType :: JRapierType -> RapierType
> fromJRapierType ( JRapierType a ) = a

> instance JSON JRapierType where
>     showJSON ( JRapierType rtype ) = toJSStrVal ( show rtype )
>     readJSON ( JSString str ) = getRtype ( reads str' )
>         where
>         str' = fromJSString str
>         getRtype [] = Error ( "Invalid typestring '" ++ str' ++ "'" )
>         getRtype ( ( rtype, rest ) : xs )
>             | eatSpace rest == [] = Ok ( JRapierType rtype )
>             | otherwise           = getRtype xs
>         eatSpace [] = []
>         eatSpace ts@( x : xs )
>             | isSpace x = eatSpace xs
>             | otherwise = ts
>     readJSON _ = Error "Type must be a string."

***

There's an instance for special field lists (because they are
implemented as objects in JSON).

> newtype JSpecialFieldList = JSpecialFieldList [ SpecialField ]

> fromJSpecialFieldList :: JSpecialFieldList -> [ SpecialField ]
> fromJSpecialFieldList ( JSpecialFieldList a ) = a

> instance JSON JSpecialFieldList where
>     showJSON = makeObj . makeValuesJS . fromJSpecialFieldList
>     readJSON ( JSObject obj ) =
>         -- Convert an object into a key-value map
>         -- then map stringify over it raising the Maybe to surround
>         -- the entire list, then massage into a Result of the
>         -- correct type.  Phew
>         ( toResult . mapM stringify . fromJSObject ) obj
>         where
>         stringify ( a, JSString str ) = Just ( a, fromJSString str )
>         stringify _                   = Nothing
>         toResult Nothing    = Error "Special fields must be strings"
>         toResult ( Just a ) = Ok ( JSpecialFieldList a )
>     readJSON _ = Error "Special field list must be an object"


***

Last, but by no means least, is the ObjectSpec instance.

> newtype JObjectSpec = JObjectSpec ObjectSpec

> fromJObjectSpec :: JObjectSpec -> ObjectSpec
> fromJObjectSpec ( JObjectSpec a ) = a

> instance JSON JObjectSpec where
>     showJSON ( JObjectSpec ( ObjectSpec name
>                                         metadata
>                                         fields
>                                         specials
>                            )
>              ) =
>         makeObj ( maybeMetadata metadata
>                   ++ maybeSpecials specials
>                   ++ [ ( "name"  , toJSStrVal name    ),
>                        ( "fields", showJSON   jfields )
>                      ]
>                 )
>         where
>           jfields = map JObjectFieldDef fields
>           maybeMetadata ( NoMetadata ) = []
>           maybeMetadata md             = [ ( "metadata",
>                                              showJSON ( JMetadata
>                                                         md
>                                                       )
>                                            )
>                                          ]
>           maybeSpecials []             = []
>           maybeSpecials specs          = makeValuesJS specs
>     readJSON ( JSObject obj ) =
>         do name     <- stringFromObj "name"   obj
>            metadata <- ( valFromObj  "metadata" obj
>                          :: Result JMetadata
>                        ) `defaultingTo` JMetadata NoMetadata
>            fields   <- valFromObj    "fields" obj
>                     :: Result [ JObjectFieldDef ]
>            specials <- ( valFromObj  "specials" obj
>                          :: Result JSpecialFieldList
>                        ) `defaultingTo` JSpecialFieldList []
>            return ( JObjectSpec
>                     ( ObjectSpec
>                       name
>                       ( fromJMetadata metadata )
>                       ( map fromJObjectFieldDef fields )
>                       ( fromJSpecialFieldList specials )
>                     )
>                   )
>     readJSON _ = Error "Object specification must be an object."


Sourcing a specification from some JSON
---------------------------------------

And now, the easy task of putting the hundreds of lines of code above
into good use.  Thankfully, users of this object specification source
don't need to know about all the fun with newtypes we've just had.

> source :: String -> Either String ObjectSpec
> source = unJ . resultToEither . decodeStrict
>     where
>       unJ ( Left  a ) = Left a
>       unJ ( Right a ) = Right ( fromJObjectSpec a )