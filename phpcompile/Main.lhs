> module Main where
> import Rapier.Obgen.Php.Compile
> import Rapier.Obgen.Php.Types

Main program
------------

The compiler can be used as a standalone program, taking in an
algebraic data-type representation of a PHP class and spitting out the
corresponding PHP code.

> main :: IO ()
> main = do
>        input <- getContents
>        let spec = (read input)::Php
>        putStr (compile spec)