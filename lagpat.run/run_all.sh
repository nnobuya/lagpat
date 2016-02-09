#! /bin/sh

rm -rf ./res
rm -rf ./anim

for n in `seq 1 2`
do
    echo $n

    cd ./in
    rm -rf  ./lagpat.in
    ln -s   ./lagpat.in.$n  ./lagpat.in
    cd ../

    mkdir ./res
    mkdir ./res/anim
    mkdir ./anim

    time ./lagpat

    mv ./anim ./anim.$n
    mv ./res  ./res.$n
done

rm -rf ./in/lagpat.in

exit

