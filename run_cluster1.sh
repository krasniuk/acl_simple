#! /bin/bash

appName=acl_simple

pathFile=$(realpath $0)
pathDir=$(dirname $pathFile)

cd $pathDir


find="\[\'acl_simple_cluster1\@mykhailo-linux-1\'\]"
insert="\[\'acl_simple_main\@mykhailo-linux-1\'\]"
perl -pi -e 's/'$find'/'$insert'/g' config/sys.config

find1="acl_simple_main"
insert1="acl_simple_cluster1"
perl -pi -e 's/'$find1'/'$insert1'/g' config/vm.args


rebar3 as prod release
cd ./_build/prod/rel/$appName/bin/
./$appName console