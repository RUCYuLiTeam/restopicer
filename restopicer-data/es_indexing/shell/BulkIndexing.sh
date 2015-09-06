#!/bin/bash
#change to bash under cygwin
if [ $# -ne 1 ];then
	echo "example: BulkIndexing.sh paper_with_issue_2014"
	exit 1;
fi
regrex=$1
path="/output"
location="http://127.0.0.1:9200"
for indexname in $(ls $path | grep $regrex)
do
	echo $location/$indexname
	curl -s -XPUT $location/$indexname --data-binary @indexmodel.json
	for file in $(ls $path/$indexname)
	do
		echo $path/$indexname/$file
		curl -s -H 'content-type': 'application/json;charset=utf8' -XPOST $location/$indexname/paper/_bulk --data-binary @$path/$indexname/$file > /dev/null
	done
done
