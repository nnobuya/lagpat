#! /usr/bin/env python

import subprocess
import sys


proc = subprocess.Popen(['ls', './res'],
                        stdout = subprocess.PIPE,
                        stderr = subprocess.PIPE)

stdout_data = proc.communicate()

list = stdout_data[0].split()


for mdl in range(len(list)):

    n_pt = []; f_pt = []

    for line in open('./res/' + list[mdl]):
        dat = line.split()
        ndat = (len(dat) - 6) /2
        tmp1 = []; tmp2 =[]
        for i in range(ndat):
            tmp1.append(dat[6 + 2*i])
            tmp2.append(dat[6 + 2*i + 1])

        n_pt.append(tmp1)
        f_pt.append(tmp2)

    
    name = list[mdl].split('_')[-1]
    out = open('./pt_list_' + name, 'w')

    for i in range(len(n_pt)):
        out.write('{0:>10}'.format(n_pt[i][0]) + '\n')

    out.close()

exit()
