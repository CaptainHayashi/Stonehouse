#!/bin/sh

# Given an input directory and an output directory, treats all JSON
# files in the input directory as Rapier object specifications in JSON
# format and compiles them all to PHP classes in the correct directory
# structure in the output directory.

# Usage:
# alljson.sh SOURCE_DIR DEST_DIR

# Thou shalt have two arguments
if [ $# != 2 ]
then
    echo "Usage: alljson.sh SOURCE_DIR DEST_DIR"
    exit
fi

# Rename these to something a bit nicer
SOURCE_DIR=$1
DEST_DIR=$2

### BEGIN TWEAKABLE BIT ###

# This is, by default, set up so that alljson.sh can be run without
# installing Stonehouse (and, indeed, it won't work otherwise).
#
# TODO: make this more elaborate so the whole thing can be installed
# properly?

BUILD_DIR=dist/build

JSON_SOURCE_BIN=rapier-jsonsource
JSON_SOURCE_DIR=${BUILD_DIR}/${JSON_SOURCE_BIN}
JSON_SOURCE=${JSON_SOURCE_DIR}/${JSON_SOURCE_BIN}

PHP_GENERATE_BIN=rapier-phpgenerate
PHP_GENERATE_DIR=${BUILD_DIR}/${PHP_GENERATE_BIN}
PHP_GENERATE=${PHP_GENERATE_DIR}/${PHP_GENERATE_BIN}

PHP_COMPILE_BIN=rapier-phpcompile
PHP_COMPILE_DIR=${BUILD_DIR}/${PHP_COMPILE_BIN}
PHP_COMPILE=${PHP_COMPILE_DIR}/${PHP_COMPILE_BIN}

GET_DIRECTORY_BIN=rapier-getdirectory
GET_DIRECTORY_DIR=${BUILD_DIR}/${GET_DIRECTORY_BIN}
GET_DIRECTORY=${GET_DIRECTORY_DIR}/${GET_DIRECTORY_BIN}

### END TWEAKABLE BIT ###

# Make a temporary directory in DEST_DIR.
mkdir -p ${DEST_DIR}/${TEMP_DIR}

for FILE in ${SOURCE_DIR}/*.json
do
    # We'll use this as a temporary storage place:
    TEMP_FN=${DEST_DIR}/temp.rob

    # Convert to temporary rob
    ${JSON_SOURCE} < ${FILE} > ${TEMP_FN}

    # Get the directory the rob should be put in
    INTO_DIR=${DEST_DIR}/`${GET_DIRECTORY} < ${TEMP_FN}`

    # Make it
    mkdir -p ${INTO_DIR}

    # And place the compiled PHP in it!
    ${PHP_GENERATE} < ${TEMP_FN} \
        | ${PHP_COMPILE} > ${INTO_DIR}/Object.php
done