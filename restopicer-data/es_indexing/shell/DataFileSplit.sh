#!/bin/bash
data_dir="/data"
path="/data_split"
bulksize=100000
for foldername in $(ls $data_dir)
do
	echo $foldername
	gzip -d $data_dir/$foldername/*.gz
	if [ ! -d $path/$foldername ]; then
		mkdir -p $path/$foldername
	fi
	for file in $(ls $data_dir/$foldername)
	do
		echo $file
		split -l $bulksize $data_dir/$foldername/$file $path/$foldername/$file
		rm -f $data_dir/$foldername/$file
	done
done
