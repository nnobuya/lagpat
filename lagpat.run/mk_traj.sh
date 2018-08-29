#! /bin/sh

if [ $# -ne 3 ]; then
    echo 'Type start # to finish #: e.g., ./run_all.sh L1.00 1 2'
    exit
fi

if [ ! -e ./mk_traj.in ]; then
    echo "no mk_traj.in file!!"
    exit
fi

. ./mk_traj.in

#----- run lagpat ------------------------------------------ #
if $run_lagpat; then

    echo '- run lagpat'

    rm -rf ./res

    for n in `seq $2 $3`
    do
	echo $n

	rm -rf ./res.$n
	mkdir -p ./res.$n/anim ./res.$n/anim ./res.$n/lpt

	ln -sf ./res.$n ./res

	cd ./in
	ln -sf  ./lagpat.in.$n  ./lagpat.in
	cd ../

	exit

	time ./lagpat

    done

    rm ./in/lagpat.in
fi


#----- run lagpat ------------------------------------------ #
if $run_traj; then

    echo '- run traj'

    rm -rf ./traj
    rm -rf ./lpt

    for n in `seq $2 $3`
    do

	rm  -rf ./traj.$n
	mkdir   ./traj.$n

	ln -sf ./traj.$n ./traj

	rm -rf ./res
	mkdir  ./res

	ln -sf ./res.$n ./lpt

	time ./lpt_post

	mv ./res/peak.dat          ./res.$n/
	mv ./res/pt_eject_nse.dat  ./res.$n/
	mv ./res/bad_traj.dat      ./res.$n/
    done

    rm -f  ./lpt
    rm -rf ./res

fi


if $run_abund; then

    rm -rf ./inicomp
    mkdir  ./inicomp

    cd ./inicomp

    mkdir ./in
    mkdir ./abund

    # set code and data
    ln -s $HOME/code/lagpat/inicomp/nse.cur/nse

    cd ./in
    ln -s $HOME/code/lagpat/inicomp/nse.cur/in/part.z4071
    ln -s $HOME/code/lagpat/inicomp/nse.cur/in/part.ame.fz4421
    cd ../

    ln -s ../pt_eject_nse.dat ./table.in

    ./nse

    cd ../
fi


if $run_eject; then

    ./ejecta_fujib.py $2 $3

    ln -sf ./pt_eject_nse.dat ./pt_eject.dat

    ./yes_map.py $1

    for no in `seq 0 3`
    do
	./set_calc.py  $no
	mv  ./pt_list.dat  ./pt_list_$no.dat 
    done
fi


if $run_ntwk_file; then

    for no in `seq 0 3`
    do
	rm -rf ./hydro.in ./abund.in ./hydro.in.$no ./abund.in.$no
	mkdir  ./hydro.in ./abund.in

	./pt_ntwk_set.py $no

	mv ./hydro.in ./hydro.in.$no
	mv ./abund.in ./abund.in.$no
    done
fi


exit
