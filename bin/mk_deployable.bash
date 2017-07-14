#!/bin/bash

ROOT_DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd .. && pwd )



if [ $# -eq 1 ]; then
    DEPLOY_DIR_ROOT=$1
else
    DEPLOY_DIR_ROOT=/tmp
fi

if [ ! -d $DEPLOY_DIR_ROOT ]; then
    echo "$DEPLOY_DIR_ROOT does not exist."
    exit 1
fi

DEPLOY_DIR=$DEPLOY_DIR_ROOT/airport-groups

if [ -d $DEPLOY_DIR ]; then
    if [ -d $DEPLOY_DIR.backup ]; then
	echo "$DEPLOY_DIR.backup will be overwritten"
    fi
    cp -rf $DEPLOY_DIR $DEPLOY_DIR.backup
    #    echo "$DEPLOY_DIR already exists"
    rm -r $DEPLOY_DIR
    # exit 1
fi

# if [ "$(ls -A $DEPLOY_DIR)" ]; then
#   echo "$DEPLOY_DIR is not empty."
#   exit 1
# fi

mkdir -p $DEPLOY_DIR/bin
mkdir -p $DEPLOY_DIR/assets

SEARCH_DIR=$ROOT_DIR/backend/airport-groups/.stack-work/install

EXECUTABLE=$( find $SEARCH_DIR  -name airport-group-service-exe | grep install)
#echo EXECUTABLE: $EXECUTABLE
cp $EXECUTABLE $DEPLOY_DIR/bin
cp $ROOT_DIR/backend/airport-groups/misc/airports_stg.txt $DEPLOY_DIR/assets
cp $ROOT_DIR/frontend/web-elm/index.html $DEPLOY_DIR/assets

cd $DEPLOY_DIR_ROOT
tar -cvzf airport-groups_$(date '+%Y.%m.%d-%H.%M.%S').tgz airport-groups

# echo done
