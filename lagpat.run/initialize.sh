#! /bin/sh

path=$HOME/code/lagpat/

echo 'all clean'

# renewal directory
rm -rf  ./res*  ./anim* ./traj*

## execute
rm -f ./lagpat
ln -s $path/lagpat

rm -f ./mk_traj.sh
ln -s $path/lagpat.run/mk_traj.sh

rm -f ./lpt_post
ln -s $path/post/lpt_post


exit
