#! /usr/bin/env python

import subprocess
import sys


nselec = 1

proc = subprocess.Popen(['ls', './res'],
                        stdout = subprocess.PIPE,
                        stderr = subprocess.PIPE)

stdout_data = proc.communicate()

list = stdout_data[0].split()


for mdl in range(len(list)):

    ## check file name
    if list[mdl][0:5] != 'large':
        continue

    n_pt = []; f_pt = []; rma_pt = []; ye_pt = []; en_pt = []

    for line in open('./res/' + list[mdl]):

        dat = line.split()
        ndat = (len(dat) - 6) /2

        ye_pt.append(float(dat[2]))
        en_pt.append(float(dat[3]))

        n_in = []; f_in = []
        for i in range(ndat):
            n_in.append(  int(dat[6 + 2*i    ]))
            f_in.append(float(dat[6 + 2*i + 1]))

        rma_pt.append(sum(f_in))

        nout = min(nselec,len(n_in))
        #print(tmp1[0:nout])

        tmp1 = []; tmp2 = []
        for i in range(nout):
            tmp1.append(n_in[i])
            tmp2.append(f_in[i])
        n_pt.append(tmp1)
        f_pt.append(tmp2)
    
    name = list[mdl].split('_')[-1]
    out  = open('./res/pt_list_' + name, 'w')

    total_ej = 0.0

    for i in range(len(n_pt)):
        for j in range(len(n_pt[i])):
            mass_ej  = rma_pt[i] /float(len(n_pt[i]))
            total_ej += mass_ej 
            out.write('{0:>10}{1:15.7e}'.format(n_pt[i][j], mass_ej))
            out.write('{0:15.7e}{1:15.7e}'.format(ye_pt[i], en_pt[i]) + '\n')
    out.close()

    print(total_ej)

exit()
