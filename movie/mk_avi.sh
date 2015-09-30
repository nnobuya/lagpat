#! /bin/sh

mencoder mf://./jpg/*.jpg -mf type=jpg:fps=10 -o pt.motion.avi -ovc lavc -lavcopts vcodec=wmv1
mencoder mf://./jpg/*.jpg -mf type=jpg:fps=10 -o pt.motion.mpg -ovc lavc -lavcopts vcodec=wmv1

exit
