#! /usr/bin/env python

import subprocess
import numpy as np
import sys


if   len(sys.argv) == 1:
    nselec = 1
elif len(sys.argv) == 2:
    nselec = int(sys.argv[1])
else:
    print('bad arguments')
    exit()

print('Set nselec = ' + str(nselec))


list = ['./large.dat']

no = []

for mdl in range(len(list)):

    n_pt = []; f_pt = []; rma_pt = []; ye_pt = []; en_pt = []


    for line in open(list[mdl]):

        dat = line.split()
        ndat = (len(dat) - 6) /2

        ye_pt.append(float(dat[2]))
        en_pt.append(float(dat[3]))

        n_in = []; f_in = []
        for i in range(ndat):
            n_in.append(  int(dat[6 + 2*i    ]))
            f_in.append(float(dat[6 + 2*i + 1]))

        rma_pt.append(sum(f_in))

        sort = np.argsort(f_in)[::-1]

        tmp1 = []; tmp2 = []; icount = 0
        for i in sort:
            tmp1.append(n_in[i])
            tmp2.append(f_in[i])

            icount += 1
            if icount == nselec: break

        n_pt.append(tmp1)
        f_pt.append(tmp2)
    
    out  = open('./pt_list.dat', 'w')

    total_ej = 0.0

    for i in range(len(n_pt)):
        for j in range(len(n_pt[i])):
            no.append(n_pt[i][j])
            mass_ej  = rma_pt[i] /float(len(n_pt[i]))
            total_ej += mass_ej 
            out.write('{0:>15}{1:20.10e}'.format(n_pt[i][j], mass_ej))
            out.write('{0:20.10e}{1:20.10e}'.format(ye_pt[i], en_pt[i]) + '\n')
    out.close()

    print(total_ej)

exit()

print(' - make nse list')

for line in open('./pt_eject.dat'):
    dat = line.split()
    if dat[0] == '#': continue

    no_nse.append(int(dat[0]))

print(no[:10])

exit()
