#! /usr/bin/env python

import subprocess
import sys

if len(sys.argv) != 3:
    print('usage:')
    print('     put 2 integers; num-of-tracers and run-directories')
    print('      ./prep_run.py 1500 64')
    exit()
else:
    n_pt = int(sys.argv[1])
    ndir = int(sys.argv[2])

nrun = n_pt /ndir
ndif = n_pt - ndir *nrun

fin = 0
for i in range(ndir):
    n = i + 1
    no = '{0:02d}'.format(n)

    ini = fin + 1
    if n <= ndif:
        fin = ini + nrun
    else:
        fin = ini + nrun - 1

    print('  ' + no + ': ' + str(ini) + ' --> ' + str(fin))

    run_dir = 'ntwk.' + no

    subprocess.call(['rm', '-rf', run_dir])
    subprocess.call(['cp', '-r', 'ntwk.cur', run_dir])
    cmd = ['sed', '-i', '-e', '', run_dir + '/run.sh']

    cmd[-2] = 's/no_st=00000/no_st=' + '{0:05d}'.format(ini)+ '/g'
    subprocess.call(cmd)

    cmd[-2] = 's/no_ed=00000/no_ed=' + '{0:05d}'.format(fin)+ '/g'
    subprocess.call(cmd)


exit()

