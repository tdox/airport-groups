#!/bin/bash

ROOT_DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd .. && pwd )


hash stack 2>/dev/null || { echo >&2 "This script requires stack (https://docs.haskellstack.org) but it's not installed.  Aborting."; exit 1; }


cd $ROOT_DIR/backend/airport-groups
stack setup
stack build

hash elm 2>/dev/null || { echo >&2 "This script requires elm (http://elm-lang.org) but it's not installed.  Aborting."; exit 1; }

cd $ROOT_DIR/frontend/web-elm
elm make airport-group-editor.elm
