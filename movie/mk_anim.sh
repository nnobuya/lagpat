#! /bin/sh


nskip=10
nini=11
#nfin=100

n=$nini
m=0
nout=11

while read time
do
    if [ $n -eq $nout ]; then
	m=$(( $m + 1 ))
	no1=`printf "%.4d" $m`
	no2=`printf "%.4d" $n`

	gnuplot<<EOF
se te post eps color font 'Times, 18'
#se te pdf font 'Times, 18'
se ou "./eps/pt.${no1}.eps"
se si sq
se xr [0:2000]
se yr [0:2000]
se xtics 0, 500
se ytics 0, 500
se mxtics 5
se mytics 5
se cbr [0.15:0.55]
#se palette define (0 'red', 0.5 'blue')
se tit '$time ms'
p '../res/anim/anim_${no2}.dat' u (\$1*1.e-5):(\$2*1.e-5):6 lc palette ps 0.2 pt 7 noti
EOF
	nout=$(( $nout + $nskip ))
    fi

    n=$(( $n + 1 ))
done < ../res/anim_set.dat

exit

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
