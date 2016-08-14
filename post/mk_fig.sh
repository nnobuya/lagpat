#! /bin/sh

mdl_list='L0.40 L0.50 L0.75 L1.00 L1.25'

if [ $# -ne 1 ]; then
    echo 'run_all.sh: needs argument for run mode: i.e, ./run_all convert/plot'
    exit
fi

for mdl in $mdl_list
do
    echo $mdl
    for n in `seq 50 50 2500`
    do
	no=`printf "%04d" $n`

	ln -sf ../../Efix_hydro_plot/MRIrun60m_$mdl/eus$no.dat ./in.dat
	if   [ $1 = 'convert' ]; then
	    ./conv
	    mv ./res/hydro_new.dat ./data_conv/hydro_${mdl}_$no.dat
	    mv ./res/shock.dat     ./data_conv/shock_${mdl}_$no.dat
	elif [ $1 = 'plot' ]; then
	    time=$(( $n/10 ))

	    ln -sf ../data_conv/hydro_${mdl}_$no.dat ./res/hydro_new.dat
            ln -sf ../data_conv/shock_${mdl}_$no.dat ./res/shock.dat
	    ./hydro_map.py $mdl $time
	    mv ./fig/ye.pdf  ./fig/ye_${mdl}_${no}.pdf
	    mv ./fig/e.pdf   ./fig/e_${mdl}_${no}.pdf
	    mv ./fig/bet.pdf ./fig/bet_${mdl}_${no}.pdf
	else
	    echo 'run_all.sh: bad argument, i.e., ./run_all convert/plot'	    
	    exit
	fi
    done
done

rm -rf in.dat
rm ./res/hydro_org.dat

exit
