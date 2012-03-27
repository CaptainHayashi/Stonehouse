PHP compiler stub
=================

Part of Stonehouse, the Rapier Object Code Generator

This file is under the public domain.

> module Main ( main ) where
> import Rapier.Obgen.Php.Compile ( compile )
> import Rapier.Obgen.Php.Types ( Php )

Main program
------------

The compiler can be used as a standalone program, taking in an
algebraic data-type representation of a PHP class and spitting out the
corresponding PHP code.

> main :: IO ()
> main = do
>        input <- getContents
>        let spec = read input :: Php
>        putStr ( compile spec )