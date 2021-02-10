#!/bin/bash

ROOT_DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd ../.. && pwd )
DEPLOY_DIR=$ROOT_DIR/target/airport-groups-webapp
# BIN_DIR=$DEPLOY_DIR/bin

# cd $BIN_DIR
cd $DEPLOY_DIR
./bin/airport-group-service
