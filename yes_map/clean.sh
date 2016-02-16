#! /bin/sh

list_dir='in fig res'

for dir in $list_dir
do
    rm -rf $dir
    mkdir $dir
done

exit
