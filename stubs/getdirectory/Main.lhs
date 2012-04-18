Directory namer stub
====================

Part of Stonehouse, the Rapier Object Code Generator

This file is under the public domain.

> module Main
>     ( main -- IO ()
>     )
> where
> import Rapier.Obgen.Object
>     ( directoryName
>     )

Main program
------------

This stub reads in a Rapier object specification and spits out the
directory that any

> main :: IO ()
> main = interact ( directoryName . read )
