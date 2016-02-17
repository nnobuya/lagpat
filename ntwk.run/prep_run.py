#! /usr/bin/env python

import ConfigParser
import subprocess

config = ConfigParser.ConfigParser()
config.readfp(open('./prep.in'))

ndir = config.getint('param', 'ndir')
n_pt = config.getint('param', 'n_pt')
path = config.get('param', 'path')
reac = config.get('param', 'reac')
ntwk_in = config.get('param', 'ntwk_in')


rate_reac = 'rate.' + reac
part_reac = 'part.' + reac

ntwk = path + '/ntwk'


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

    print(' ' *5 + no + ': ' + '{0:>5d}'.format(ini) 
          + '  --> ' + '{0:>5d}'.format(fin))

    run_dir = 'ntwk_' + no

    subprocess.call(['rm', '-rf', run_dir])
    subprocess.call(['mkdir', run_dir])

    subprocess.call(['ln', '-s', ntwk   , run_dir])
    subprocess.call(['mkdir', run_dir + '/in'])
    subprocess.call(['ln', '-s', path + '/in/reac', run_dir + '/in'])
    subprocess.call(['ln', '-s', path + '/in/ty87', run_dir + '/in'])
    subprocess.call(['ln', '-s', path + '/in/fiss', run_dir + '/in'])
    subprocess.call(['ln', '-s', path + '/in/dat', run_dir + '/in'])
    subprocess.call(['ln', '-s', path + '/in/TStamp.log', run_dir + '/in'])

    subprocess.call(['ln', '-s', path + '/in/reac/' + rate_reac,
                     run_dir + '/in/rate.in'])

    subprocess.call(['ln', '-s', path + '/in/reac/' + part_reac,
                     run_dir + '/in/part.in'])

    subprocess.call(['cp', ntwk_in, run_dir + '/in'])
    subprocess.call(['mkdir', run_dir + '/in/init'])
    subprocess.call(['mkdir', run_dir + '/res'])


    subprocess.call(['cp', './run.sh', run_dir])


    cmd = ['sed', '-i', '-e', '', run_dir + '/run.sh']

    cmd[-2] = 's/no_st=00000/no_st=' + '{0:05d}'.format(ini)+ '/g'
    subprocess.call(cmd)

    cmd[-2] = 's/no_ed=00000/no_ed=' + '{0:05d}'.format(fin)+ '/g'
    subprocess.call(cmd)


exit()

