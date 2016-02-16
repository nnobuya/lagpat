#! /usr/bin/env python


rad_bnd_in = 2.e7

mdl_list = ['../res.1', '../res.2']

npt = 0
ye = []; en = []; rma = []; no = []
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
            rad = float(dat[6])
            if rad >= rad_bnd_in:
                Read = True
        else:
            Read = False

        if Read:
            no.append(npt)
            ye.append(float(dat[14]))
            en.append(float(dat[15]))
            rma.append(float(dat[5]))

print('- total particles    : ' + '{0:10d}'.format(npt))
print('- total ejecta       : ' + '{0:10d}'.format(len(ye)))
print('- ejected mass (Msun): ' + '{0:10.3e}'.format(sum(rma)))


out = open('./pt_eject.dat', 'w')

for i in range(len(ye)):
    out.write('{0:>10d}'.format(no[i]))
    out.write('{0:14.5e}{1:14.5e}{2:14.5e}'.format(ye[i], en[i], rma[i]) + '\n')
out.close()


exit()
