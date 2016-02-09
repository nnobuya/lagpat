#! /bin/sh

rm -rf ./res
mkdir  ./res

for no in `seq 1 2`
do
    rm ./lpt
    ln -s ../res.$no  ./lpt

    rm -rf  ./tracer
    mkdir   ./tracer

    time ./lpt_post

    rm -rf ./tracer.$no
    mv     ./tracer   ./tracer.$no

    cd ./res
    mv  ./hydro_nse.dat   ./hydro_nse.$no.dat  
    mv  ./peak.dat        ./peak.$no.dat
    mv  ./pt_eject.dat    ./pt_eject.$no.dat
    cd ../
done

exit
