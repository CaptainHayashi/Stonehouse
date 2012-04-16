Rapier.Types
============

Part of the Haskell Rapier Common Library

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

This module contains the algebraic data types for Rapier's type
system, Read and Show instances for Rapier types, and functions for
creating and manipulating data representing Rapier types.

> module Rapier.Types
>     ( RapierType ( .. )
>     , stringToType         -- :: String -> RapierType
>     , enumChoices          -- :: RapierType -> Maybe [ String ]
>     , recursiveType        -- :: RapierType -> Maybe RapierType
>     , isTSBracket          -- :: Char -> Bool
>     , prop_pipeInverse1    -- :: String -> Bool
>     , prop_pipeInverse2    -- :: String -> Bool
>     , prop_readShowInverse -- :: RapierType -> Bool
>     )
> where
> import Control.Monad
>     ( msum
>     , ap
>     )
> import Control.Arrow
>     ( first
>     , ( *** )
>     )
> import Control.Applicative
>     ( ( <*> )
>     , pure
>     )
> import Data.Char
>     ( isSpace
>     )
> import Data.List
>     ( intercalate
>     , stripPrefix
>     , elemIndex
>     )
> import Data.Maybe
>     ( fromMaybe
>     , catMaybes
>     )
> import Rapier.Utils
>     ( escape   -- Used for escapePipe and unescapePipe
>     , unescape
>     , onHead
>     , safeHead
>     )


Type declaration
================

Following is an enumeration of all the different Rapier types, two of
which (the vector and the map) are recursive.

> data RapierType = RpInteger
>                 | RpNatural
>                 | RpReal
>                 | RpDate
>                 | RpTime
>                 | RpString
>                 | RpBoolean
>                 | RpEnum String [ String ] -- head and tail of list
>                 | RpList RapierType
>                 | RpMap RapierType
>                 | RpObject String
>                 | RpArray Int RapierType
>                 | RpMaybe RapierType
>                   deriving Eq


Safe constructors
=================

enum, passed a list of choices, returns a RpEnum with those choices if
and only if the list has at least one item, and Nothing if it does
not.

> enum :: [ String ] -> Maybe RapierType
> enum []         = Nothing
> enum ( x : xs ) = Just ( RpEnum x xs )


Making type data
================

The main way we'll expose of creating a Rapier type is to express it
as a typestring and use the stringToType function, which is
(currently) simply a wrapper for RapierType's (to be defined) read
instance.

> stringToType :: String -> RapierType
> stringToType = read


Convenience functions
=====================

enumChoices takes a RapierType and, if it describes an Enum, returns
the list of choices attached to it.

> enumChoices :: RapierType -> Maybe [ String ]
> enumChoices ( RpEnum x xs ) = Just ( x : xs )
> enumChoices _               = Nothing

***

recursiveType takes a RapierType and, if it is recursive, returns the
RapierType contained within it.

> recursiveType :: RapierType -> Maybe RapierType
> recursiveType ( RpMap     a ) = Just a
> recursiveType ( RpList    a ) = Just a
> recursiveType ( RpArray _ a ) = Just a
> recursiveType ( RpMaybe   a ) = Just a
> recursiveType _               = Nothing


Read and Show
-------------

The Show and Read instances for RapierType are defined such that
RapierTypes are always expressed as their Rapier typestrings.

***

We'll override Show and Read to use the Rapier typestring system in a
bit, but first let's define a binding between scalar Rapier types and
their typestrings.

This version, scalarNames, is useful for Show.

> scalarNames :: [ ( RapierType, String ) ]
> scalarNames =  [ ( RpInteger, "Integer" )
>                , ( RpNatural, "Natural" )
>                , ( RpReal,    "Real"    )
>                , ( RpDate,    "Date"    )
>                , ( RpTime,    "Time"    )
>                , ( RpString,  "String"  )
>                , ( RpBoolean, "Boolean" )
>                ]

The flipped version of this, mapping typestrings to names, is useful
for Read.

> nameScalars :: [ ( String, RapierType ) ]
> nameScalars = map ( uncurry ( flip (,) ) ) scalarNames


We also have a similar table for parsing compound Rapier types.  Our
table will map opening brackets to tuples of close brackets and
parsing functions, and is used for Read.

In order to have a single point of truth, we have a system of entries
containing the open and close brackets of given types as well as the
parse function for processing those types, and we use them later on
when we need to know the brackets and parsers.

Each ParseEntry is arranged so that they can be looked up in a list by
open bracket, which is useful for Read.

> type Parser     = String -> Maybe RapierType
> type ParseEntry = ( Char, ( Char, Parser ) )

> enumEntry, mapEntry, vectorEntry, maybeEntry, objectEntry
>     :: ParseEntry
> enumEntry   = makeEntry '(' ')'  parseEnum
> mapEntry    = makeEntry '{' '}'  parseMap
> vectorEntry = makeEntry '[' ']'  parseVector
> maybeEntry  = makeEntry '<' '>'  parseMaybe
> objectEntry = makeEntry '`' '\'' parseObject

> makeEntry :: Char -> Char -> Parser -> ParseEntry
> makeEntry open close parse = ( open, ( close, parse ) )

Getting the individual items out of a ParseEntry:

> openBrk, closeBrk :: ParseEntry -> Char
> openBrk = fst
> closeBrk = fst . snd

These are all collated into one table for Read.

> compoundEntries :: [ ParseEntry ]
> compoundEntries = [ enumEntry
>                   , mapEntry
>                   , vectorEntry
>                   , maybeEntry
>                   , objectEntry
>                   ]

The brackets defined above aren't allowed to be anywhere else in a
valid Rapier typestring, so here's a function to check to see if a
character is a bracket (we'll use this for tests later).

> isTSBracket :: Char -> Bool
> isTSBracket chr = any isThisBracket compoundEntries
>     where
>     isThisBracket ( open, ( close, _ ) )
>         | chr == open  = True
>         | chr == close = True
>         | otherwise    = False

In addition, there are three other items that are put here but aren't
part of the parse entry set: the separator character separating enum
terms (pipe), the character used to denote a "wildcard" vector length
(the vector is actually a List), and the separator character
separating the length of an vector from its type (lengthDelim).

> pipe, lengthWildcard, lengthDelim :: Char
> pipe           = '|'
> lengthWildcard = '*'
> lengthDelim    = 'x'


***

Now define Show.

Firstly, let's anticipate a problem with the Rapier typestring
encoding of the pipe character, |.  If we allow any pipes in input
strings to be shown as pipes verbatim, any typestring parser will
become very confused when trying to read those pipes in later -- it'll
think they're delimiting enum choices!

This is solved by a pair of utility functions, escape and unescape,
defined in Rapier.Utils; we can pass them pipe to make them escape and
unescape pipe characters.

***

So that we can use our scalarNames set above, we match against all the
non-scalar types first, so that the general match can fall back to
iterating through scalarNames.

> instance Show RapierType where
>     -- Parametric types
>     show ( RpObject a    ) = encaps objectEntry a
>     show ( RpEnum   a as ) = encaps enumEntry   bs
>         where
>         bs = intercalate [ pipe ] ( map ( escape pipe ) ( a : as ) )
>     -- Recursive types
>     show ( RpList   a    ) = showVector [ lengthWildcard ] a
>     show ( RpArray  n a  ) = showVector ( show n ) a
>     show ( RpMap    a    ) = encaps mapEntry   ( show a )
>     show ( RpMaybe  a    ) = encaps maybeEntry ( show a )
>     -- Scalar types
>     show x = fromMaybe
>              ( error "Unknown type" )
>              ( lookup x scalarNames )

encaps takes a ParseEntry and a string, and encapsulates the string in
the brackets contained within the ParseEntry.

> encaps :: ParseEntry -> String -> String
> encaps entry str = ( openBrk entry : str ) ++ [ closeBrk entry ]

And this function generates a typestring for a vector (list or array)
with the given (shown) length.

> showVector :: String -> RapierType -> String
> showVector len rtype =
>     encaps vectorEntry ( len ++ ( lengthDelim : show rtype ) )


***

Now for the trickier part, making a typestring parser for read.  This
time, we do things the other way around, trying scalars first (because
it's easier to parse them than to parse anything else!).  We then go
onto compound (parametric and recursive) types if our scalar parser
fails, and then onto an error message.

The type parser is guaranteed to return at most one possible parse.

Firstly, we'll define a helpful function, lsep, which separates a list
at the leftmost occurrence of the given item, returning the list
segments surrounding it as a tuple.  The whole thing is wrapped in a
Maybe; the result is Nothing if the element does not exist.

It is defined in terms of a general function, nsep, which removes the
element at a given position in the list and splits the two adjacent
sublists.

> lsep :: ( Eq a ) => a -> [ a ] -> Maybe ( [ a ], [ a ] )
> lsep x lst = nsep ( elemIndex     x lst ) lst

> nsep :: Maybe Int -> [ a ] -> Maybe ( [ a ], [ a ] )
> nsep Nothing    _   = Nothing
> nsep ( Just n ) lst = Just ( take n lst, drop ( n + 1 ) lst )

***

A problem found in earlier iterations of the Read instance was that it
would fail to parse Read strings with whitespace padding.  The padding
must first be stripped so that our parser can accept these - thus
stripSpace.

> stripSpace :: String -> String
> -- We don't need to strip trailing whitespace, hopefully.
> stripSpace = dropWhile isSpace

***

> instance Read RapierType where
>     readsPrec _ = catMaybes . ( parsers <*> ) . pure . stripSpace
>         where parsers = [ parseScalar, parseCompound ]


Trying to parse against scalars is simple: for each scalar in our
scalar table, we see if we can strip the scalar's typestring as a
prefix of the to-parse string.  If we can, we use >>= to push the
result into a tuple with the scalar type; if we can't, we get Nothing.
mplus stops computation when the result becomes a Just, so the first
correct parse (if any) is returned.

If we run out of scalars and still haven't got anywhere, then the
parse failed and we'll move onto parametric types.

> parseScalar :: String -> Maybe ( RapierType, String )
> parseScalar str =
>     msum [ do rest <- stripPrefix ts str
>               return ( rtype, rest )
>            | ( ts, rtype ) <- nameScalars
>          ]


Now for the compound types.  We get our foot in the door by looking at
the first character of the input to see if it's a bracket.  If we
match an opening bracket, we hand the rest of the string over to an
intermediate function that checks for the closing bracket and, if one
exists, runs the parser on the typestring in between.

> parseCompound :: String -> Maybe ( RapierType, String )
> parseCompound lst =
>     do x <- safeHead lst -- Will stop computation if list is empty
>        ( c, f ) <- lookup x compoundEntries
>        balanceBrackets x c f ( tail lst )

balanceBrackets, given an opening bracket and a close bracket, walks
through the string incrementing a counter (initially set to 1)
whenever it sees the opening bracket and decrementing it whenever it
sees the closing bracket.

If it reaches a state in which the counter is zero before the string
ends, the given parsing function will be launched with the walked
string provided and the result, if parsing succeeds, will be fed into
a tuple with the remaining string less the final bracket.

If balanceBrackets exhausts the entire string before reaching
equilibrium, Nothing is returned.

(Note that we assume that any parametric types inside this typestring
are devoid of brackets.)

> balanceBrackets :: Char -> Char
>                    -> ( String -> Maybe RapierType )
>                    -> String
>                    -> Maybe ( RapierType, String )
> balanceBrackets open close f = iter ( 1 :: Int ) []
>     where -- Attempts to simplify this have been thwarted so far
>     iter 0 inner outer      = doParse ( init inner, outer )
>     iter _ _     []         = Nothing
>     iter i inner ( x : xs ) = iter ( g i ) ( inner ++ [ x ] ) xs
>         where g | x == open  = ( + 1 )
>                 | x == close = subtract 1
>                 | otherwise  = id
>     doParse = propMaybe . first f
>     propMaybe ( Just a , b  ) = Just ( a, b )
>     propMaybe ( Nothing, _  ) = Nothing

***

For Enum, we want to split the input by | to get the list of possible
strings for the given Enum, and wrap them in RpEnum.  At the same
time, we want to avoid splitting by "\|" (escaped pipe characters)!

We do this by iterating over the list whilst keeping a list of
currently parsed choices, with the head of this list always
representing the choice currently being parsed.  Whenever we see | and
have just parsed a \\, we add a | to this choice; whenever we see an
unescaped |, we add a new head and start parsing another choice;
whenever we see anything else, it just gets added onto the head.
Heads are built up in reverse, so we flip them when we're done with
them, and flip the first one when we get the result back from iter.

Because this pushes the parsed choices on in the wrong order, we then
reverse the entire list afterwards.

> parseEnum :: Parser
> parseEnum = enum . reverse . onHead reverse . foldl iter [ "" ]
>     where
>     iter ( current : parsed ) nextChr
>          | nextChr /= pipe = ( nextChr : current ) : parsed
>          | otherwise =
>              case current of
>                       ( '\\' : _ ) -> ( pipe : current ) : parsed
>                       _            -> "" : reverse current : parsed
>     iter _ nextChr = [ [ nextChr ] ] -- Start sequence, appease ghc

Object is substantially easier - we don't need to do any parsing on
the internal parameter.

> parseObject :: Parser
> parseObject = Just . RpObject

Finally, we get to the recursive types.

List and Array share the same typestring format, differentiated only
by the fact that the latter includes a number denoting the length of
the array, and List instead has * (a wildcard length).  This makes
parsing a little complicated, but nothing we can't handle.

This parser function is a tiny bit flimsy and doesn't do much
checking.

> parseVector :: Parser
> parseVector str =
>     maybe ( error ( "expected : in " ++ str ) ) -- No separator
>           ( Just . parse )                      -- Separator
>           ( lsep lengthDelim str )
>     -- parse takes in (length identifier, inner rtype)
>     -- gets (rtype constructor, inner rtype) and applies fst to snd
>         where
>         parse = ap fst snd . ( constructor *** stringToType )
>         constructor "*" = RpList
>         constructor i   = RpArray ( read i :: Int )

Finally, Map and parseMaybe are pretty simple.

> parseMap, parseMaybe :: Parser
> parseMap   = Just . RpMap   . stringToType
> parseMaybe = Just . RpMaybe . stringToType


Properties
----------

The read and show parsers we just defined are quite complex even for
the simple typestring grammar, so it makes sense to define some
properties they must obey for the purposes of testing.

***

"For all Rapier types t, doing a round-trip conversion (show-ing
followed by read-ing) must not change the type."  (In other words,
read and show must be exact inverses.)

Note: This property doesn't necessarily hold when the parameters of a
parametric type are malformed (eg, Enum is passed weird control
characters as one or more of the choices).  This property may need to
be refined later.

> prop_readShowInverse :: RapierType -> Bool
> prop_readShowInverse = ap ( == ) ( stringToType . show )

***

"escape pipe and unescape pipe must be inverses, so long as unescape
pipe is not performed on a string with unescaped pipes."

The second property needs refining before testing by removing any
generated test cases in which there are unescaped pipes.

> prop_pipeInverse1, prop_pipeInverse2 :: String -> Bool
> prop_pipeInverse1 = ap ( == ) ( unescape pipe . escape pipe )
> prop_pipeInverse2 = ap ( == ) ( escape pipe . unescape pipe )