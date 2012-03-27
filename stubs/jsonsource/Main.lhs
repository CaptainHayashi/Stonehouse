JSON source stub
================

Part of Stonehouse, the Rapier Object Code Generator

This file is under the public domain.

> module Main ( main ) where
> import Rapier.Obgen.Json.Source ( source )

Main program
------------

This stub takes a JSON representation of a Rapier object and converts
it into a format that can be used with Stonehouse's code generator
stubs.

> main :: IO ()
> main = do
>   input <- getContents
>   putStr ( either id
>                   show
>                   ( source input )
>          )