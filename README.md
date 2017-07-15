# Airport Groups

This is a web app for defining groups of airports.

## Quick Start Guide (for Mac and linux)

Install [stack](https://docs.haskellstack.org) and [elm](http://elm-lang.org). Then

    $ cd ./src/bin
    $ ./build_and_run.bash


## Directory structure

The src directory contains three subdirectories.

* bin: contains scripts for cleaning, building, packaging and running the app.
  (clean.bash, build.bash, package.bash, run.bash)

* backend/airport-groups: backend service

* frontend/web-elm: single page frontend web app

See the README.md files in those last two directories for more detailed instructions how how to manually build and run the web app using stack and elm.

## Overview

* Example programs are given in src/backend/airport-groups/test/programs.
* A help summary is given in src/backend/airport-groups/misc/help.txt
* The language for defining airport groups is specified in src/backend/airport-groups/misc/lang.txt
