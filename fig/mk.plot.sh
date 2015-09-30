#! /bin/sh

echo 'se term post eps enhanced color' > rad.gp
echo "se out 'rad.eps'"          >> rad.gp
echo 'se log y'                  >> rad.gp
echo 'se yrange [10:4000]'       >> rad.gp
echo 'se xrange [0:0.45]'        >> rad.gp
echo "se xlabel 'time'"          >> rad.gp
echo "se ylabel 'radius'"        >> rad.gp
echo 'se style line 1 lc 1 lt 1' >> rad.gp
echo "p \\"                      >> rad.gp

n=2
until [ $n -gt 250 ]
do
    echo "'./fort.99' u 1:${n} w l ls 1 noti,\\" >> rad.gp
    n=`expr $n + 3 `
done
    echo "'./fort.99' u 1:${n} w l noti lt 1" >> rad.gp

gnuplot ./rad.gp

exit
