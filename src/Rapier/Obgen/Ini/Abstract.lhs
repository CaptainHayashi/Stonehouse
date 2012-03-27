

This module converts, via lexing and parsing, a string containing a
Windows-style INI configuration set into an abstract definition of
that configuration format.

It is intended to be used in conjunction with the Convert module,
which converts the abstract definition into a format-neutral
representation, to convert an INI file into an object specification
useable with the object generator backends.

> module Rapier.Obgen.Ini.Abstract where

ADT for concrete INI syntax
---------------------------

> data IniToken = StartSectionName
>               | EndSectionName
>               | Equals

Lexical analyser
----------------

The lexer part of this 

> lexIni :: String -> [IniToken]
> lexIni str = lexIniInner str []

> lexIniInner :: String -> [IniToken] -> [IniToken]

BASE CASE: If there is no more input to read, return the tokens
scanned.

> lexIni [] output = output

INDUCTIVE CASES: Look for patterns 

> lexIni 