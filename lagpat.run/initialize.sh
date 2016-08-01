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
rm -f ./lagpat
ln -s $path/lagpat

rm -f ./mk_traj.sh
ln -s $path/lagpat.run/mk_traj.sh

rm -f ./lpt_post
ln -s $path/post/lpt_post

rm -f ./ejecta_sawai.py
ln -s $path/post/ejecta_sawai.py

rm -f ./yes_map.py
ln -s $path/yes_map/yes_map.py

rm -f ./set_calc.py
ln -s $path/yes_map/set_calc.py

exit
