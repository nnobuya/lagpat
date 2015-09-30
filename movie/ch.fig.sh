#! /bin/sh


n=0

until [ $n -gt 178 ]
#until [ $n -gt 10 ]
do

    m=$(( 178 - $n ))

    no=`printf "%.5d" $n`
    no_rev=`printf "%.5d" $m`

    echo $no


    epsf="./eps/pt.${no}.eps"
    pngf="./png/pt.${no}.png"
    jpgf="./jpg/pt.${no}.jpg"

    jpgf_rev="./jpg/pt.${no_rev}.jpg"

    convert -density 400 $epsf $pngf

    n=$(( $n + 1 ))
done


exit
