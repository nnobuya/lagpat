#! /usr/bin/env python

import subprocess

dir_path = './abund.in'

no_pt1 = 40000

subprocess.call(['rm', '-rf', dir_path])
subprocess.call(['mkdir', dir_path])


no2 = 0

for line in open('./pt_list.dat'):
    no2 += 1

    no1 = int(line.split()[0])

    if no1 <= no_pt1:
        in_path = './res/abund.1'
    else:
        in_path = './res/abund.2'
        no1 = no1 - no_pt1

    file1 = in_path + '/abund.'  + '{0:07d}'.format(no1) + '.in'
    file2 = dir_path + '/abund_' + '{0:05d}'.format(no2) + '.dat'

    print(file1 + ' --> ' + file2)
    subprocess.call(['cp', file1, file2])

exit()
