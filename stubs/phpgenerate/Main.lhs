PHP generator stub
==================

Part of Stonehouse, the Rapier Object Code Generator

This file is under the public domain.

> module Main ( main ) where
> import Rapier.Obgen.Php.Generate ( generatePhp )

Main program
------------

The generator can be used as a standalone program, taking in an Obgen
algebraic data-type representation of a Rapier object specification
and spitting out an ADT representation of the PHP class implementing
it.

> main :: IO ()
> main = interact ( ( maybe
>                     ( error "Could not generate PHP class." )
>                     show
>                   )
>                   . generatePhp . read
>                 )

