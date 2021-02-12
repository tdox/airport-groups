#!/bin/bash

ROOT_DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd ../.. && pwd )


hash stack 2>/dev/null || { echo >&2 "This script requires stack (https://docs.haskellstack.org) but it's not installed.  Aborting."; exit 1; }

SRC_DIR=$ROOT_DIR/src


echo building the backend
cd $SRC_DIR/backend
stack setup
stack build
pwd

hash elm 2>/dev/null || { echo >&2 "This script requires elm (http://elm-lang.org) but it's not installed.  Aborting."; exit 1; }

echo making the front end
cd $SRC_DIR/frontend
elm make src/Main.elm --output=target/main.js

# elm make --yes airport-group-editor.elm
