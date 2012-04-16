Stonehouse test suite
=====================

Part of Stonehouse, the Rapier Object Code Generator

This file is under the public domain.

> module Main ( main ) where
> import Control.Monad ( liftM
>                      , liftM2
>                      )
> import Data.List ( elemIndices )
> import Data.Char ( isPrint )
> import Test.QuickCheck
> import Test.Framework ( Test
>                       , defaultMain
>                       )
> import Test.Framework.Providers.QuickCheck2 ( testProperty )
> import Rapier.Types ( RapierType ( .. )
>                     , isTSBracket
>                     , prop_pipeInverse1
>                     , prop_pipeInverse2
>                     , prop_readShowInverse
>                     )

Test boilerplate for Rapier.Obgen.Object

> instance Arbitrary RapierType where
>    arbitrary =
>        oneof [ return RpInteger,
>                return RpNatural,
>                return RpReal,
>                return RpTime,
>                return RpString,
>                return RpBoolean,
>                liftM2 RpEnum arbitrary arbitrary,
>                liftM RpList arbitrary,
>                liftM RpObject arbitrary,
>                liftM RpMap arbitrary,
>                liftM RpMaybe arbitrary,
>                liftM2 RpArray arbitrary arbitrary
>              ]

Refining of prop_pipeInverse2 to make sure strings with unescaped
pipes aren't tested.

> prop_pipeInverse2' :: String -> Property
> prop_pipeInverse2' a =
>     noUnescapedPipes ==> prop_pipeInverse2 a
>     where
>       noUnescapedPipes = checkAt ( elemIndices '|' a )
>       checkAt [] = True
>       checkAt ( x : xs )
>           | x == 0               = False
>           | a !! (x - 1) /= '\\' = False
>           | otherwise            = checkAt xs

Refining of prop_readShowInverse to make sure only printable strings
not containing brackets are tested.  (There seem to be some problems
with regards to strings containing control characters that cause
massive refutations, and brackets inside parameters are banned in
Rapier)

> prop_readShowInverse' :: RapierType -> Property
> prop_readShowInverse' a =
>     onlyValidStrings a ==> prop_readShowInverse a
>         where
>         onlyValidStrings ( RpEnum   x xs ) = checkStrings ( x : xs )
>         onlyValidStrings ( RpObject x    ) = checkString x
>         onlyValidStrings ( RpList   x    ) = onlyValidStrings x
>         onlyValidStrings ( RpMap    x    ) = onlyValidStrings x
>         onlyValidStrings ( RpMaybe  x    ) = onlyValidStrings x
>         onlyValidStrings ( RpArray  _ x  ) = onlyValidStrings x
>         onlyValidStrings _               = True
>         checkStrings [] = True
>         checkStrings ( x : xs )
>             | checkString x == True = checkStrings xs
>             | otherwise             = False
>         checkString [] = True
>         checkString ( x : xs )
>             | isTSBracket x      = False
>             | isPrint x == False = False
>             | otherwise          = checkString xs

> main :: IO ()
> main = defaultMain tests

> tests :: [ Test ]
> tests = [ testProperty "rapier-type/pipeInverse1" prop_pipeInverse1,
>           testProperty "rapier-type/pipeInverse2" prop_pipeInverse2',
>           testProperty "rapier-type/inverse"      prop_readShowInverse'
>         ]