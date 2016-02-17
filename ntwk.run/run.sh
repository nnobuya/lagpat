#! /bin/sh

no_st=00000
no_ed=00000


res_list= 'ntwkout fini fend phys reac'

for no in `seq -w  $no_st  $no_ed`
do

    echo 'run #'$no

    ### set files
    rm -f  ./in/init/abund.in
    rm -f  ./in/init/hydro.in

    ln -s ../abund.in/abund_$no.dat   ./in/init/abund.in
    ln -s ../hydro.in/hydro_$no.dat   ./in/init/hydro.in

    echo " -------------------- now running -------------------- "
    time ./ntwk > ../res/run/run.$no.dat

    for name in $res_list
    do
	mv  ./res/$name.dat   ./res/$name/$name.$no.dat
    done

    mv ./res/fend.dec  ./res/fend/fend.$no.dec
done

rm -f  ./in/init/abund.in
rm -f  ./in/init/hydro.in

exit
