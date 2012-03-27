PHP generator stub
==================

Part of Stonehouse, the Rapier Object Code Generator

This file is under the public domain.

> module Main ( main ) where
> import Rapier.Obgen.Php.Generate ( generatePhp )
> import Rapier.Obgen.Object ( ObjectSpec )
> import Data.Maybe ( fromMaybe )

Main program
------------

The generator can be used as a standalone program, taking in an Obgen
algebraic data-type representation of a Rapier object specification
and spitting out an ADT representation of the PHP class implementing
it.

> main :: IO ()
> main = do
>        input <- getContents
>        let spec = read input :: ObjectSpec
>        print ( fromMaybe
>                ( error "Could not generate PHP class." )
>                ( generatePhp spec )
>              )