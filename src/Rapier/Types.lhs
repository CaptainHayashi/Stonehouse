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

Licencing mumbo-jumbo aside...

This module contains the algebraic data types for Rapier's type
system, Read and Show instances for Rapier types, and functions for
creating and manipulating data representing Rapier types.

> module Rapier.Types ( RapierType ( .. ),
>                       stringToType,
>                       -- :: String -> RapierType
>                       enumChoices,
>                       -- :: RapierType -> Maybe [ String ]
>                       recursiveType,
>                       -- :: RapierType -> Maybe RapierType
>                       prop_pipeInverse1,
>                       -- :: String -> Bool
>                       prop_pipeInverse2,
>                       -- :: String -> Bool
>                       prop_readShowInverse,
>                       -- :: RapierType -> Bool
>                     ) where
> import Data.List ( intercalate,
>                    stripPrefix,
>                    elemIndex,
>                    foldl',
>                  )
> import Data.Char ( isSpace )
> import Data.Maybe ( fromMaybe )


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
>                 | RpEnum String [ String ]
>                 | RpList RapierType
>                 | RpMap RapierType
>                 | RpObject String
>                 | RpArray Int RapierType
>                   deriving Eq


Making type data
================

The main way we'll expose of creating a Rapier type is to express it
as a typestring and use the stringToType function, which is
(currently) simply a wrapper for RapierType's (to be defined) read
instance.

> stringToType :: String -> RapierType
> stringToType ts = read ts :: RapierType


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
> recursiveType _               = Nothing


Read and Show
=============

The Show and Read instances for RapierType are defined such that
RapierTypes are always expressed as their Rapier typestrings.


Preliminary work
----------------

We'll override Show and Read to use the Rapier typestring system in a
bit, but first let's define a binding between scalar Rapier types and
their typestrings.

This version, scalarNames, is useful for Show.

> scalarNames :: [ ( RapierType, String ) ]
> scalarNames =  [ ( RpInteger, "Integer" ),
>                  ( RpNatural, "Natural" ),
>                  ( RpReal,    "Real"    ),
>                  ( RpDate,    "Date"    ),
>                  ( RpTime,    "Time"    ),
>                  ( RpString,  "String"  ),
>                  ( RpBoolean, "Boolean" ) ]

The flipped version of this, mapping typestrings to names, is useful
for Read.

> nameScalars :: [ (String, RapierType) ]
> nameScalars = map flipPair scalarNames
>     where
>       flipPair ( a, b ) = ( b, a )


Show
----

Now define Show.

Firstly, let's anticipate a problem with the Rapier typestring
encoding of the pipe character, |.  If we allow any pipes in input
strings to be shown as pipes verbatim, any typestring parser will
become very confused when trying to read those pipes in later -- it'll
think they're delimiting enum choices!

Therefore, what we need to do is be able to escape those pipes (with a
backslash, \|) and unescape them later.  Two functions, escapePipe and
unescapePipe, will do the trick.

> escapePipe :: String -> String
> escapePipe = foldl' escapePipeInner ""
>     where
>     escapePipeInner current '|' = current ++ "\\|"
>     escapePipeInner current x   = current ++ [ x ]

> unescapePipe :: String -> String
> unescapePipe = iter ""
>     where
>     iter result []                  = result
>     iter result ( '\\' : '|' : xs ) = iter ( result ++ "|"   ) xs
>     iter result ( x          : xs ) = iter ( result ++ [ x ] ) xs

An arising property of these two functions is that they must work as
inverses.  (Strictly speaking, escapePipe is unescapePipe's inverse if
and only if there are no unescaped pipes already present.)

> prop_pipeInverse1 :: String -> Bool
> prop_pipeInverse1 a = ( unescapePipe . escapePipe ) a == a

The below property needs refining before testing by removing any
generated test cases in which there are unescaped pipes.

> prop_pipeInverse2 :: String -> Bool
> prop_pipeInverse2 a = ( escapePipe . unescapePipe ) a == a


***

So that we can use our scalarNames set above, we match against all the
non-scalar types first, so that the general match can fall back to
iterating through scalarNames.

> instance Show RapierType where
>     -- Parametric types
>     show ( RpEnum a as ) = encapsPar '(' as' ')'
>         where
>         as' = intercalate "|" ( map escapePipe ( a : as ) )
>     show ( RpObject a  ) = encapsPar '<' a '>'
>     -- Recursive types
>     show ( RpList   a  ) = showVector "*" a
>     show ( RpMap    a  ) = encapsRec '{' a '}'
>     show ( RpArray n a ) = showVector (show n) a
>     -- Scalar types
>     show x = fromMaybe (error "Unknown type") (lookup x scalarNames)

The two convenience functions for encapsulating parametric or
recursive types in their bracket characters:

> encapsPar :: Char -> String -> Char -> String
> encapsPar first rest end = (first : rest) ++ [ end ]

> encapsRec :: Char -> RapierType -> Char -> String
> encapsRec first rest = encapsPar first (show rest)

And this function generates a typestring for a vector (list or array)
with the given (shown) length.

> showVector :: String -> RapierType -> String
> showVector len rtype = '[' : len ++ ":" ++ show rtype ++ "]"


Read
----

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
stripWhitespace.

> stripWhitespace :: String -> String
> -- We don't need to strip trailing whitespace, hopefully.
> stripWhitespace = dropWhile isSpace

***

> instance Read RapierType where
>     readsPrec _ str = [ tryScalar ]
>         where
>         tryScalar   = fromMaybe tryCompound ( parseScalar str'   )
>         tryCompound = fromMaybe tryFail     ( parseCompound str' )
>         tryFail     = error ( "Could not parse " ++ str' )
>         str'        = stripWhitespace str

Trying to parse against scalars is simple: we just go through the
mappings of typestrings to scalar types, see if they form a prefix of
the string and, if they do, then we've found a result.  (We can do
this easily by forcibly trying to strip the typestring out of the
input and using maybe to massage the outcome.)

If we run out of scalars and still haven't got anywhere, then the
parse failed and we'll move onto parametric types.

> parseScalar :: String -> Maybe ( RapierType, String )
> parseScalar str = tryScalarsRecursively nameScalars
>     where
>     tryScalarsRecursively [] = Nothing
>     tryScalarsRecursively ( ( ts, rtype ):xs ) =
>         maybe ( tryScalarsRecursively xs ) -- If parse fails
>               ( \x -> Just ( rtype, x ) )  -- If parse succeeds
>               ( stripPrefix ts str )       -- Try parsing here

Now for the compound types.  We get our foot in the door by looking at
the first character of the input to see if it's a bracket.  If we
match an opening bracket, we hand the rest of the string over to an
intermediate function that checks for the closing bracket and, if one
exists, runs the parser on the typestring in between.

> parseCompound :: String -> Maybe ( RapierType, String )
> parseCompound ( '<':xs ) = balanceBrackets '<' '>' parseObject xs
> parseCompound ( '(':xs ) = balanceBrackets '(' ')' parseEnum   xs
> parseCompound ( '[':xs ) = balanceBrackets '[' ']' parseVector xs
> parseCompound ( '{':xs ) = balanceBrackets '{' '}' parseMap    xs
> parseCompound _          = Nothing

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
> balanceBrackets open close f = iter 1 []
>     where
>     iter :: Int -> String -> String -> Maybe ( RapierType, String )
>     iter 0 inner outer = doParse ( init inner, outer )
>     iter _ _     []    = Nothing
>     iter i inner ( x : xs ) = iter i' ( inner ++ [ x ] ) xs
>         where i' | x == open  = i + 1
>                  | x == close = i - 1
>                  | otherwise  = i
>     doParse ( ins, outs ) =
>         maybe Nothing                    -- Failure? Throw Nothing.
>               ( \y -> Just ( y, outs ) ) -- Success? Pair with outs
>               ( f ins )                  -- Run the parser!

***

For Enum, we want to split the input by | to get the list of possible
strings for the given Enum, and wrap them in RpEnum.  At the same
time, we want to avoid splitting by "\|" (escaped pipe characters)!

We do this by iterating over the list whilst keeping a list of
currently parsed choices, with the head of this list always
representing the choice currently being parsed.  Whenever we see \|,
we add a | to this choice; whenever we see |, we add a new head and
start parsing another choice; whenever we see anything else, it just
gets added onto the head.

Because this pushes the parsed choices on in the wrong order, we then
reverse the entire list afterwards.

> parseEnum :: String -> Maybe RapierType
> parseEnum str = Just ( RpEnum c cs )
>     where
>     ( c : cs ) = reverse ( iter [ "" ] str )
>     iter [] a = iter [ "" ] a -- Stop GHC from whining.
>     iter results [] = results
>     -- Escaped pipe
>     iter ( current : parsed ) ( '\\' : '|' : xs ) =
>         iter ( ( current ++ "|"  ) : parsed ) xs
>     -- Actual pipe
>     iter ( current : parsed ) ( '|'        : xs ) =
>         iter ( "" : current : parsed ) xs
>     -- Anything else
>     iter ( current : parsed ) ( x          : xs ) =
>         iter ( (current ++ [ x ] ) : parsed ) xs


Object is substantially easier - we don't need to do any parsing on
the internal parameter.

> parseObject :: String -> Maybe RapierType
> parseObject = Just . RpObject

Finally, we get to the recursive types.

List and Array share the same typestring format, differentiated only
by the fact that the latter includes a number denoting the length of
the array, and List instead has * (a wildcard length).  This makes
parsing a little complicated, but nothing we can't handle.

This parser function is a tiny bit flimsy and doesn't do much
checking.

> parseVector :: String -> Maybe RapierType
> parseVector str =
>     maybe ( error ( "expected : in " ++ str ) ) -- No separator
>           parse                                 -- Separator
>           ( lsep ':' str )
>         where
>         parse ( "*", rtype ) = Just ( RpList     ( rd rtype ) )
>         parse ( i  , rtype ) = Just ( RpArray i' ( rd rtype ) )
>             where i' = read i :: Int
>         rd x = read x :: RapierType

Finally, Map is pretty simple.

> parseMap :: String -> Maybe RapierType
> parseMap str = Just ( RpMap ( read str )::RapierType )


Properties of Read and Show
---------------------------

The read parser we just defined is quite complex even for the simple
typestring grammar, so it makes sense to define some properties it
must obey for the purposes of testing.

The simplest property is thus:

"For all Rapier types t, doing a round-trip conversion (show-ing
followed by read-ing) must not change the type."  (In other words,
read and show must be exact inverses.)

Note: This property doesn't necessarily hold when the parameters of a
parametric type are malformed (eg, Enum is passed weird control
characters as one or more of the choices).  This property may need to
be refined later.

> prop_readShowInverse :: RapierType -> Bool
> prop_readShowInverse rtype =
>     rtype == ( read ( show rtype ) :: RapierType )