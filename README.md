Stonehouse
==========

Stonehouse is a collection of Unixy filters written in Haskell for
converting a Rapier object (ROB) specification into to code for a
Rapier implementation, for example a class definition or a C
structure.

Currently implemented
---------------------

PHP: Code generator (phpgenerate) and compiler (phpcompile).
JSON: Specification source (jsonsource).

To be implemented
-----------------

Java: Code generator (javagenerate) and compiler (javacompile).

What is Rapier?
---------------

Rapier is a URY project to build a relatively simple, integrated
remote data retrieval service (essentially doing similar work to an
object-relational mapper, an object request broker, and an
authentication system).

Licencing
---------

The main source distribution of Stonehouse (everything under src/) is
under the 3-clause BSD licence (see COPYING).

Program stubs (stubs/) and the test suite (test/) are in the public
domain when legally possible due to their triviality.

Usage
-----

Stonehouse is a Cabal package - just use the normal Cabal-Install
framework to build and install it.

Stonehouse is mainly built up of a library, as well as small
program-lets ("stubs") that implement a Unix filter/pipeline interface
to said library.  For example, to build a PHP class file from a
Haskell dump of a Rapier ObjectSpec:

    $ phpgenerate <dump.rob | phpcompile >dump.php

The name
--------

It generates code for Rapier Objects (ROBs).  Rob Stonehouse was the
Station Manager in 2012.  Enough said!
