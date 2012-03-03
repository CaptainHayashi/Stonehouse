#!/bin/sh

dist/build/phpgenerate/phpgenerate < $1 | dist/build/phpcompile/phpcompile
