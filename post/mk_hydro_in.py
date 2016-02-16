#! /usr/bin/env python

import subprocess

dir_path = './hydro.in'

no_pt1 = 40000

subprocess.call(['rm', '-rf', dir_path])
subprocess.call(['mkdir', dir_path])


no2 = 0

for line in open('./in/pt_list.dat'):
    no2 += 1

    no1 = int(line)

    if no1 <= no_pt1:
        in_path = './tracer.1'
    else:
        in_path = './tracer.2'
        no1 = no1 - no_pt1

    file1 = in_path + '/hydro_' + '{0:05d}'.format(no1) + '.dat'
    file2 = dir_path + '/hydro_' + '{0:05d}'.format(no2) + '.dat'

    print(file1 + ' --> ' + file2)
    subprocess.call(['cp', file1, file2])

exit()
