#! /bin/sh

if [ $# -ne 2 ]; then
    echo 'Type start # to finish #: e.g., ./run_all.sh 1 2'
    exit
fi

rm -rf ./res
rm -rf ./anim

for n in `seq $1 $2`
do
    echo $n

    cd ./in
    rm -rf  ./lagpat.in
    ln -s   ./lagpat.in.$n  ./lagpat.in
    cd ../

    mkdir ./res
    mkdir ./res/anim
    mkdir ./res/lpt
    mkdir ./anim

    time ./lagpat

    mv ./anim ./anim.$n
    mv ./res  ./res.$n
done

rm -rf ./in/lagpat.in

exit

