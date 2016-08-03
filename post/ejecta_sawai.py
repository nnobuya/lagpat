#! /usr/bin/env python

import sys


if len(sys.argv) != 3:
    exit('You need to specify models.\n   e.g. ./ejecta_sawai.py 1 2')

mdl_list = []
for n in range(int(sys.argv[1]), int(sys.argv[2])+1):
    mdl_list.append('./res.' + str(n))

rad_bnd_in = 2.e7


npt = 0; de =[]; te = []; rd = []; ye = []; en = []; rma = []; no = []; ti = []
for n in range(len(mdl_list)):

    for line in open(mdl_list[n] + '/part_fini.dat'):

        dat = line.split()

        ### header
        if dat[0] == '#':
            continue

        npt += 1

        if   int(dat[4]) ==  1 and n == 0:
            Read = True
        elif int(dat[4]) != -1 and n == 1:
            rd_in = float(dat[6])
            if rd_in >= rad_bnd_in:
                Read = True
        else:
            Read = False

        if Read:
            vr = float(dat[9])
            if vr > 0:
                no.append(npt)

                ti.append(0.0)

                rd.append(float(dat[6]))
                de.append(float(dat[12]))
                te.append(float(dat[13]))
                ye.append(float(dat[14]))
                en.append(float(dat[15]))
                rma.append(float(dat[5]))


    print('  number of ejecta {0:2d}: {0:7d}'.format(n+1,npt))

print('-'*50)

print('- total particles    : ' + '{0:10d}'.format(npt))
print('- total ejecta       : ' + '{0:10d}'.format(len(ye)))
print('- ejected mass (Msun): ' + '{0:10.3e}'.format(sum(rma)))


out = open('./pt_eject_fini.dat', 'w')

out.write('#{0:>9}'.format('no'))
out.write('{0:>18}'.format('Mass'))
out.write('{0:>18}'.format('Time'))
out.write('{0:>18}'.format('Density'))
out.write('{0:>18}'.format('Temperature'))
out.write('{0:>18}'.format('Entropy'))
out.write('{0:>18}'.format('Ye'))
out.write('{0:>18}\n'.format('Radius'))

for i in range(len(ye)):
    out.write('{0:>10d}'.format(no[i]))
    out.write('{0:18.10e}'.format(rma[i]))
    out.write('{0:18.10e}'.format(ti[i]))
    out.write('{0:18.10e}'.format(de[i]))
    out.write('{0:18.10e}'.format(te[i]))
    out.write('{0:18.10e}'.format(en[i]))
    out.write('{0:18.10e}'.format(ye[i]))
    out.write('{0:18.10e}'.format(rd[i]))
    out.write('\n')

out.close()

# eject NSE

out = open('./pt_eject_nse.dat', 'w')

out.write('#{0:>9}'.format('no'))
out.write('{0:>18}'.format('Mass'))
out.write('{0:>18}'.format('Time'))
out.write('{0:>18}'.format('Density'))
out.write('{0:>18}'.format('Temperature'))
out.write('{0:>18}'.format('Entropy'))
out.write('{0:>18}'.format('Ye'))
out.write('{0:>18}\n'.format('Radius'))

nse_line = []
for n in range(len(mdl_list)):
    for line in open(mdl_list[n] + '/pt_eject_nse.dat'):
        nse_line.append(line)

for i in no:
    out.write(nse_line[i-1])


out.close()

exit()
