> module Main where
> import Rapier.Obgen.Php.Generate
> import Rapier.Obgen.Object

Main program
------------

The generator can be used as a standalone program, taking in an Obgen
algebraic data-type representation of a Rapier object specification
and spitting out an ADT representation of the PHP class implementing
it.

> main :: IO ()
> main = do
>        input <- getContents
>        let spec = (read input)::ObjectSpec
>        print (maybe (error "Could not generate PHP class.") id (generatePhp spec))