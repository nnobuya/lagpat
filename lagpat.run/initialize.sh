#! /bin/sh

path=$HOME/code/lagpat/

echo 'all clean'

# clean files
rm -f pt_eject.dat
rm -f pt_list_*.dat
rm -f large.dat

# renewal directory
rm -rf  ./res*  ./anim* ./traj* ./eject

## execute
ln -sf $path/lagpat
ln -sf $path/lagpat.run/mk_traj.sh
ln -sf $path/post/lpt_post
#ln -sf $path/post/ejecta_sawai.py
ln -sf $path/post/ejecta_fujib.py
ln -sf $path/yes_map/yes_map.py
ln -sf $path/yes_map/set_calc.py

exit
