#!/bin/bash

ROOT_DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd ../.. && pwd )

echo $ROOT_DIR

SRC_DIR=$ROOT_DIR/src
TARGET_DIR=$ROOT_DIR/target


# if [ $# -eq 1 ]; then
#     DEPLOY_DIR_ROOT=$1
# else
#     DEPLOY_DIR_ROOT=/tmp
# fi

# if [ ! -d $DEPLOY_DIR_ROOT ]; then
#    echo "$DEPLOY_DIR_ROOT does not exist."
#    exit 1
#fi


DEPLOY_DIR=$ROOT_DIR/target/airport-groups-webapp
echo $DEPLOY_DIR


    
#DEPLOY_DIR=$DEPLOY_DIR_ROOT/airport-groups

#if [ -d $DEPLOY_DIR ]; then
#    if [ -d $DEPLOY_DIR.backup ]; then
#	echo "$DEPLOY_DIR.backup will be overwritten"
#    fi
#    cp -rf $DEPLOY_DIR $DEPLOY_DIR.backup
    #    echo "$DEPLOY_DIR already exists"
#    rm -r $DEPLOY_DIR
    # exit 1
#fi

# if [ "$(ls -A $DEPLOY_DIR)" ]; then
#   echo "$DEPLOY_DIR is not empty."
#   exit 1
# fi

BIN_DIR=$DEPLOY_DIR/bin
if [ ! -d $BIN_DIR ]; then
    mkdir -p $BIN_DIR
fi

ASSETS_DIR=$DEPLOY_DIR/assets
if [ ! -d $ASSETS_DIR ]; then
    mkdir -p $ASSETS_DIR
fi


#mkdir -p $DEPLOY_DIR/bin
#mkdir -p $DEPLOY_DIR/assets

BACKEND_DIR=$SRC_DIR/backend
SEARCH_DIR=$BACKEND_DIR/airport-groups/.stack-work/install

EXECUTABLE=$( find $SEARCH_DIR  -name airport-group-service-exe | grep install)
#echo EXECUTABLE: $EXECUTABLE
cp $EXECUTABLE $BIN_DIR
cp $BACKEND_DIR/airport-groups/assets/airports.txt $ASSETS_DIR
cp $SRC_DIR/frontend/web-elm/index.html $ASSETS_DIR

cd $TARGET_DIR
tar -cvzf airport-groups-webapp_$(date '+%Y.%m.%d-%H.%M.%S').tgz airport-groups-webapp

# echo done
