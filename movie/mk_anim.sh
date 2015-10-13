#! /bin/sh


n=10

until [ $n -gt 380 ]
#until [ $n -gt 10 ]
do

    no=`printf "%.5d" $n`

    echo $no

    epsf="./eps/pt.${no}.eps"
    pngf="./png/pt.${no}.png"
    jpgf="./jpg/pt.${no}.jpg"

    gnuplot<<EOF
se te post eps color 'Helvetica' 18
se ou "./eps/pt.${no}.eps"
se si sq
se xr [0:1000]
se yr [0:1000]
se xtics 0, 500
se ytics 0, 500
se mxtics 5
se mytics 5
p '../res/anim.dat' i $n u (\$1*1.e-5):(\$2*1.e-5) ps 0.3 pt 7 noti
EOF

    n=$(( $n + 1 ))
done


exit
