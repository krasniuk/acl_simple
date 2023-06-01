#! /bin/bash

appName=acl_simple

pathFile=$(realpath $0)
pathDir=$(dirname $pathFile)

cd $pathDir
make rel
cd ./_build/prod/rel/$appName/bin/
./$appName console
