#! /bin/bash

model=majin

path=$HOME/code/lagpat/

echo 'all clean'

# clean files
rm -f pt_eject.dat pt_list_*.dat large.dat

rm -rf ./hydro.in.? ./abund.in. ./res*  ./anim* ./traj* ./eject

rm -rf ./in
mkdir  ./in
mkdir ntwk_data

if [ $1 = 'majin' ]
then
    cp $path/lagpat.run/in/lagpat.in.majin ./in/lagpat.in.org
else
    cp $path/lagpat.run/in/lagpat.in.org   ./in/
fi


## execute
ln -sf $path/lagpat
ln -sf $path/lagpat.run/mk_traj.sh
ln -sf $path/post/lpt_post
#ln -sf $path/post/ejecta_sawai.py
ln -sf $path/post/ejecta_$model.py  ./ejecta.py
ln -sf $path/post/pt_ntwk_set.py
ln -sf $path/yes_map/yes_map.py
ln -sf $path/yes_map/set_calc.py

### mk_traj.in
echo 'run_lagpat=false'      >  ./mk_traj.in
echo 'run_traj=false'       >> ./mk_traj.in
echo 'run_abund=false'      >> ./mk_traj.in
echo 'run_eject=false'      >> ./mk_traj.in
echo 'run_ntwk_file=false'  >> ./mk_traj.in


exit
