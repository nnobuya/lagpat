#! /bin/sh

path=$HOME/code/lagpat/

echo 'all clean'

# clean files
rm -f pt_eject.dat pt_list_*.dat large.dat

rm -rf hydro.in.? abund.in.?

# renewal directory
rm -rf  ./res*  ./anim* ./traj* ./eject

## execute
ln -sf $path/lagpat
ln -sf $path/lagpat.run/mk_traj.sh
ln -sf $path/post/lpt_post
#ln -sf $path/post/ejecta_sawai.py
ln -sf $path/post/ejecta_fujib.py
ln -sf $path/post/pt_ntwk_set.py
ln -sf $path/yes_map/yes_map.py
ln -sf $path/yes_map/set_calc.py

exit
