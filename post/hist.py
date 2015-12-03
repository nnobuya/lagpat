#! /usr/bin/env python


ye = []; en = []; ma = []
for line in open('./pt_eject.dat'):
    dat = line.split()
    ye.append(float(dat[0]))
    en.append(float(dat[1]))
    ma.append(float(dat[2]))

fac = [ r1/(sum(ma)) for r1 in ma ]

import matplotlib.pyplot as plt

plt.hist(ye, 50, weights = fac)
plt.xlim(0.2,0.4)
plt.ylim(1.e-4,0.1)
plt.yscale('log')
plt.show()

exit()
