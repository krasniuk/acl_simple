#! /bin/bash

appName=acl_simple

pathFile=$(realpath $0)
pathDir=$(dirname $pathFile)

cd $pathDir


find2="\[\'acl_simple_main\@mykhailo-linux-1\'\]"
insert2="\[\'acl_simple_cluster1\@mykhailo-linux-1\'\]"
perl -pi -e 's/'$find2'/'$insert2'/g' config/sys.config

find3="acl_simple_cluster1"
insert3="acl_simple_main"
perl -pi -e 's/'$find3'/'$insert3'/g' config/vm.args


rebar3 as prod release
cd ./_build/prod/rel/$appName/bin/
./$appName console
