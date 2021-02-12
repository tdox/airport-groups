#!/bin/bash

ROOT_DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd ../.. && pwd )


hash stack 2>/dev/null || { echo >&2 "This script requires stack (https://docs.haskellstack.org) but it's not installed.  Aborting."; exit 1; }

if [ -d $ROOT_DIR/target ]; then
    echo removing target
    rm -r $ROOT_DIR/target;
fi

SRC_DIR=$ROOT_DIR/src

cd $SRC_DIR/backend
stack clean

if [ "$1" = "-d" ]; then
    echo deep cleaning
    if [ -d .stack-work ]; then
	echo removing .stack-work
	rm -r .stack-work
    fi
fi

cd $SRC_DIR/frontend/

if [ -e index.html ]; then
    rm index.html
fi


if [ "$1" = "-d" ]; then
    if [ -d elm-stuff ]; then
	echo removing elm-stuff
	rm -r elm-stuff
    fi
fi

