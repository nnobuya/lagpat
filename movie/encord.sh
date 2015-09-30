#! /bin/sh


movie=pt_motion.mpg
rm -f  $movie
ffmpeg -r 20 -i ./png/pt.%5d.png -s 1000x1000  $movie

exit


for ph in 'de' 'te' 'ye' 'en'
do


#    movie="./mpg/${ph}.linux.mpg"
#    rm -f  $movie
#    mencoder mf://png/${ph}.*.png -mf fps=30:type=png -ovc copy -oac copy -o  $movie

    movie="./mpg/${ph}.mpg"
    rm -f  $movie
    ffmpeg -r 20 -i ./png/${ph}.%5d.png -s 1000x1000  $movie
done

exit
