#! /usr/bin/env python

import subprocess
import sys

n_total_set = 40000

if len(sys.argv) == 2:
    nlist = int(sys.argv[1])
else:
    nlist = 1

infile = './pt_list_' + str(nlist) + '.dat'

print(' adopt ' + infile)


no = []; fa = []; ye = []; et = []
for line in open(infile):
    dat = line.split()

    no.append(int(dat[0]))
    fa.append(float(dat[1]))
    ye.append(float(dat[2]))
    et.append(float(dat[3]))



total = sum(fa)
fa = [ r1 /total for r1 in fa ]

print(' - cp hydro.in and abund.in')

for i, j in enumerate(no):

    no2 = '{0:07d}'.format(i + 1)

    if j <= n_total_set:
        no1 = '{0:07d}'.format(j)
    else:
        no1 = '{0:07d}'.format(j - n_total_set)

    fl1 = './traj.1/hydro_'   + no1 + '.dat'
    fl2 = './hydro.in/hydro_' + no2 + '.dat'

    subprocess.call(['cp', fl1, fl2])


    # initial abund
    fl1 = './inicomp/abund/abund_'   + no1 + '.dat'
    fl2 = './abund.in/abund_' + no2 + '.dat'
    subprocess.call(['cp', fl1, fl2])


exit()
