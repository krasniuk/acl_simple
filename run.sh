#! /bin/bash

pathFile=$(realpath $0)
pathDir=$(dirname $pathFile)

cd $pathDir
make rel
cd ./_build/prod/rel/acl_simple/bin/
./acl_simple console
