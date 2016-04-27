#! /bin/sh

if [ $# -ne 2 ]; then
    echo 'Type start # to finish #: e.g., ./run_all.sh 1 2'
    exit
fi

run_lagpat=true
run_traj=true


#----- run lagpat ------------------------------------------ #
if $run_lagpat; then

    echo '- run lagpat'

    rm -rf ./res
    rm -rf ./anim

    for n in `seq $1 $2`
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

    for n in `seq $1 $2`
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


exit
