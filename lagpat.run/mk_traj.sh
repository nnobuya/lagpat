#! /bin/sh

if [ $# -ne 3 ]; then
    echo 'Type start # to finish #: e.g., ./run_all.sh L1.00 1 2'
    exit
fi

run_lagpat=true
run_traj=true
run_eject=true

#----- run lagpat ------------------------------------------ #
if $run_lagpat; then

    echo '- run lagpat'

    rm -rf ./res
    rm -rf ./anim

    for n in `seq $2 $3`
    do
	echo $n

	cd ./in
	rm -f  ./lagpat.in
	ln -s  ./lagpat.in.$n  ./lagpat.in
	cd ../

	mkdir ./anim
	mkdir ./res
	mkdir ./res/anim
	mkdir ./res/lpt

	time ./lagpat

	rm -rf ./anim.$n
	rm -rf ./res.$n

	mv ./anim ./anim.$n
	mv ./res  ./res.$n
    done

    rm ./in/lagpat.in
fi


#----- run lagpat ------------------------------------------ #
if $run_traj; then

    echo '- run traj'

    for n in `seq $2 $3`
    do

	rm -rf ./traj
	mkdir  ./traj

	rm -rf ./res
	mkdir  ./res

	rm -f lpt
	ln -s ./res.$n ./lpt

	time ./lpt_post

	rm  -rf     ./traj.$n
	mv  ./traj  ./traj.$n

	mv ./res/hydro_nse.dat ./res.$n/
	mv ./res/peak.dat      ./res.$n/
	mv ./res/pt_eject.dat  ./res.$n/
	mv ./res/bad_traj.dat  ./res.$n/
    done

    rm -f  ./lpt
    rm -rf ./res

fi


if $run_eject; then
    ./ejecta_sawai.py $2 $3

    ./yes_map.py $1

    for no in `seq 1 3`
    do
	./set_calc.py  $no
	mv  ./pt_list.dat  ./pt_list_$no.dat 
    done
fi

exit
