#! /usr/bin/env python

def normal(x):
    total = sum(x)
    y = [ r /total for r in x ]
    return y

Plot = False

na_all = 250
nz_all = 100

fac = []
for line in open('./pt_list.dat'):
    dat = line.split()
    if dat[0] == '#':
        continue
    fac.append(float(dat[1]))

fac = normal(fac)

nmdl =len(fac)

x = []; y = []
for mdl in range(nmdl):

    no = '{0:05d}'.format(mdl + 1)
    n = []; z = []; a = []; x_in = []; y_in = []
    for line in open('./fend.all/fend.' + no + '.dec'):
        if line[0:1] == '#':
            continue
        elif len(line) < 10:
            break

        dat = line.split()
        n.append(int(dat[0]))
        z.append(int(dat[1]))
        a.append(int(dat[2]))
        x_in.append(float(dat[3]))
        y_in.append(float(dat[4]))

    x.append(x_in)
    y.append(y_in)


x_ave = [ 0.0 for r1 in a ]
y_ave = [ 0.0 for r1 in a ]
for i in range(len(x[0])):
    for mdl in range(nmdl):
        x_ave[i] += x[mdl][i] *fac[mdl]


ia = [ i + 1 for i in range(na_all)]
xa = [ 0.0   for i in range(na_all)]

iz = [ i + 1 for i in range(nz_all)]
xz = [ 0.0   for i in range(nz_all)]

for j in range(len(ia)):
    for i in range(len(a)):
        if a[i] == ia[j]:
            xa[j] += x_ave[i]

for j in range(len(iz)):
    for i in range(len(z)):
        if z[i] == iz[j]:
            xz[j] += x_ave[i]

ya = [ r1 /float(i1) for i1, r1 in zip(ia, xa) ]
yz = [ r1 /float(i1) for i1, r1 in zip(iz, xz) ]

out = open('./fabund_ave_a.dat', 'w')
out.write('{0:<5}{1:>18}{2:>18}\n'.format('#', 'X', 'Y'))
for i in range(len(ia)):
    out.write('{0:>5d}{1:18.10e}{2:18.10e}\n'.format(ia[i], xa[i], ya[i]))
out.close()

out = open('./fabund_ave_z.dat', 'w')
out.write('{0:<5}{1:>18}{2:>18}\n'.format('#', 'X', 'Y'))
for i in range(len(iz)):
    out.write('{0:>5d}{1:18.10e}{2:18.10e}\n'.format(iz[i], xz[i], yz[i]))
out.close()

if not Plot:
    exit()

import matplotlib.pyplot as plt

plt.plot(ia, ya   , 'o-')

plt.yscale('log')
plt.xlim(0,240)
plt.ylim(1.e-7,1)

plt.xlabel('Mass number, $A$')
plt.xlabel('Abundance, $Y_A$')

plt.savefig('fabund.pdf')

exit()
