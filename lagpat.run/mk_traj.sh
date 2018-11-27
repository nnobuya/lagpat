#! /bin/bash

if [ $# -ne 4 ]; then
    echo 'Type start # to finish #: e.g., ./run_all.sh sawai L1.00 1 2'
    exit
fi

if [ ! -e ./mk_traj.in ]; then
    echo "no mk_traj.in file!!"
    exit
fi

. ./mk_traj.in

#----- run lagpat ------------------------------------------ #
if $run_lagpat; then

    echo ' ========== run lagpat ==========='

    rm -rf ./res

    for n in `seq $3 $4`
    do
	echo $n

	rm -rf ./res.$n
	mkdir -p ./res.$n/anim ./res.$n/anim ./res.$n/lpt

	ln -nsf ./res.$n ./res

	cd ./in
	ln -sf  ./lagpat.in.$n  ./lagpat.in
	cd ../

	time ./lagpat

    done

    rm -rf ./res
    rm ./in/lagpat.in
fi


#----- run traj ------------------------------------------ #
if $run_traj; then

    echo ' ========== run traj ==========='

    rm -rf  ./traj  ./lpt

    for n in `seq $3 $4`
    do

	rm  -rf ./traj.$n
	mkdir   ./traj.$n

	ln -nsf ./traj.$n ./traj

	rm -rf ./res
	mkdir  ./res

	ln -snf ./res.$n ./lpt

	time ./lpt_post

	mv ./res/peak.dat          ./res.$n/
	mv ./res/pt_eject_nse.dat  ./res.$n/
	mv ./res/bad_traj.dat      ./res.$n/
    done

    rm -rf  ./lpt ./res

fi


if $run_abund; then

    echo ' ========== run abund ==========='

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

    ln -s ../res.1/pt_eject_nse.dat ./table.in

    ./nse

    cd ../
fi


if $run_eject; then

    echo ' ========== run eject ==========='

    eject_pt=true
    pt_ntwk=true

    if $eject_pt; then
	echo ' - calculates ejecta particles: '$3' to '$4
	echo ''

	echo ' - individual set'
	for j in `seq $3 $4`
	do
	    echo '   set '$j':'

	    ./ejecta.py $1 $j $j

	    mv ./pt_eject_fini.dat ./pt_list/pt_eject_fini_$j.dat
	    mv ./pt_eject_nse.dat  ./pt_list/pt_eject_nse_$j.dat
	    echo ''
	done
	
        ### total adjust
	echo ' - total set --------'
	./ejecta.py $1 $3 $4

	mv ./pt_eject_fini.dat ./pt_list/pt_eject_fini_all.dat
	mv ./pt_eject_nse.dat  ./pt_list/pt_eject_nse_all.dat
    fi


    if $pt_ntwk; then

	echo ' - calculates pt list for nucleosynthesis: '$3' to '$4
	echo ''
	echo '   - individual'

	for j in `seq $3 $4`
	do
	    ln -sf ./pt_list/pt_eject_nse_$j.dat ./pt_eject.dat

	    ./yes_map.py $2

	    for no in `seq 1 3`
	    do
		./set_calc.py  $no
		mv  ./pt_list.dat  ./pt_list/pt_list_${j}_${no}.dat 
	    done

	    mv ./large.dat    ./pt_list/large_$j.dat
	done

	echo '   - total'

	ln -sf ./pt_list/pt_eject_nse_all.dat ./pt_eject.dat

	./yes_map.py $2
	
	for no in `seq 0 2`
	do
	    ./set_calc.py  $no
	    mv  ./pt_list.dat  ./pt_list/pt_list_all_${no}.dat 
	done

	mv ./large.dat    ./pt_list/large_all_$j.dat

	rm -rf ./pt_eject.dat

    fi
    
fi


if $run_ntwk_file; then

    echo ' ========== run ntwk files ==========='

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
