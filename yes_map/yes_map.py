#! /usr/bin/env python

import matplotlib
matplotlib.use('Agg')

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import MultipleLocator, FormatStrFormatter


Plot = True

#out = open('./ye_s.dat','w')

if False:
    mdl_list  = ['30m', '60m', 'L0.50', 'L0.75', 'L1.25']
    list_label = ['$h$-MRI',
                  '$l$-MRI',
                  '$l$-MRI-L0.50',
                  '$l$-MRI-L0.75',
                  '$l$-MRI-L1.25']
    #mdl_list   = ['30m']
    #list_label = ['$h$-MRI']
elif True:
    mdl_list   = ['L0.50']
    list_label =['$l$-MRI-L0.50']
else:
    mdl_list  = ['mri_60m', 'mri_30m']
    name_list = ['l', 'h']
    list_label = ['$l$-MRI', '$h$-MRI']


## grid setting
nye = 111
ns  = 101

fac_cut = 1.e-5

ye_grid = np.linspace(0.0,  0.55, nye)
s_grid  = np.linspace(0.0,  50.0, ns )


#################################################################
ye = []; s = []; ma = []; fac = []; no = []

for mdl in mdl_list:
    ye_in = []; s_in = []; ma_in = []; no_in = []
    for line in open('./in/pt_ej_' + mdl + '.dat'):
        dat = line.split()
        no_in.append(int(dat[0]))
        ye_in.append(float(dat[1]))
        s_in.append(float(dat[2]))
        ma_in.append(float(dat[3]))

    total  = sum(ma_in)
    fac_in = [ r1 /total for r1 in ma_in ]

    no.append(no_in)
    ye.append(ye_in)
    s.append(s_in)
    ma.append(ma_in)
    fac.append(fac_in)


print('- yes map')

for mdl in range(len(mdl_list)):

    fac_grid = [ [0.0 for col in range(nye)] for row in range(ns) ]
    pt_grid  = [ [ [] for col in range(nye)] for row in range(ns) ]
    pt_fact  = [ [ [] for col in range(nye)] for row in range(ns) ]

    for n in range(len(fac[mdl])):

        for i1 in range(nye - 1):
            if ye_grid[i1] <= ye[mdl][n] and ye[mdl][n] < ye_grid[i1 + 1]:
                break

        for i2 in range(ns-1):
            if s_grid[i2] <= s[mdl][n] and s[mdl][n] < s_grid[i2 + 1]:
                break

        pt_grid[i2][i1].append(no[mdl][n])
        pt_fact[i2][i1].append(fac[mdl][n])
        fac_grid[i2][i1] += fac[mdl][n]

    outf   = open('./res/large_' + mdl_list[mdl]  + '.dat', 'w')
    ncount = 0
    for i in range(nye):
        for j in range(ns):
            if fac_grid[j][i] >= fac_cut:
                outf.write('{0:>5}{1:>5}'.format(i,j))
                outf.write('{0:11.3e}{1:11.3e}'.format(ye_grid[i],s_grid[j]))
                outf.write('{0:11.3e}'.format(fac_grid[j][i]) + '  |')
                for n in range(len(pt_grid[j][i])):
                    outf.write('{0:>7}{1:14.5e}'.
                               format(pt_grid[j][i][n], pt_fact[j][i][n]))
                outf.write('\n')
                ncount += 1

            fac_grid[j][i] = np.log10(fac_grid[j][i] + 1.e-30)

    outf.close()

    if not Plot:
        continue

    ### format #########################
    plt.rcParams['font.serif'] = 'Times'
    plt.rcParams['font.size'] = 18

    x1, x2 = np.meshgrid(ye_grid, s_grid)

    col_range = np.linspace(-5.0, -1.0, 5)

    xmajorLocator   = MultipleLocator(0.1)
    xmajorFormatter = FormatStrFormatter('%3.1f')
    xminorLocator   = MultipleLocator(0.05)

    ymajorLocator   = MultipleLocator(10)
    ymajorFormatter = FormatStrFormatter('%d')
    yminorLocator   = MultipleLocator(5)

    fig, ax = plt.subplots()
    fig, ay = plt.subplots()

    ### format #########################

    plt.xlim(0.1,0.5)
    plt.ylim(5,40)

    plt.xticks([0.1, 0.2, 0.3, 0.4, 0.5])
    plt.yticks([0, 10, 20, 30])

    ax.xaxis.set_major_locator(xmajorLocator)
    ax.xaxis.set_major_formatter(xmajorFormatter)
    ax.xaxis.set_minor_locator(xminorLocator)

    ay.yaxis.set_major_locator(ymajorLocator)
    ay.yaxis.set_major_formatter(ymajorFormatter)
    ay.yaxis.set_minor_locator(yminorLocator)


    plt.contourf(x1, x2, fac_grid, col_range, cmap = 'hot_r')
    plt.xlabel('$Y_{\\rm e, nse} (\\Delta Y_{\\rm e} =0.05)$')
    plt.ylabel('Entropy, $S_{\\rm nse} $($\\Delta S = 0.5$),'
               + 'k$_{\\rm B}$ baryon$^{-1}$')

    plt.title('${\\it' + mdl_list[mdl] + '}$' + '-MRI')

    plt.colorbar(label='Mass fraction ($\\log_{10}$)')
    plt.savefig('./fig/yes_' + mdl_list[mdl] + '.pdf')

    plt.close()


if not Plot:
    exit()

print('plotting histogram')

yebin = [0.01*i for i in range(56)]

for i in range(len(mdl_list)):
    plt.hist(ye[i], yebin,
             alpha = 0.3, weights = fac[i], label = list_label[i])

plt.rcParams['font.serif'] = 'Times'
plt.rcParams['font.size'] = 16

plt.xlabel('$Y_{\\rm e}$')
plt.ylabel('Mass fraction')
plt.xlim(0, 0.52)
plt.ylim(1.e-3,1.0)
plt.yscale('log')
plt.legend(loc=2,fontsize = 15)

plt.savefig('./fig/hist_ye.pdf')

plt.close()

sbin = [1*i for i in range(56)]

for i in range(len(mdl_list)):
    plt.hist(s[i], sbin,
             alpha = 0.5, weights = fac[i], label = list_label[i])


xmajorLocator   = MultipleLocator(10)
xmajorFormatter = FormatStrFormatter('%d')
xminorLocator   = MultipleLocator(5)

ax.xaxis.set_major_locator(xmajorLocator)
ax.xaxis.set_major_formatter(xmajorFormatter)
ax.xaxis.set_minor_locator(xminorLocator)

plt.xticks([10, 20, 30, 40])

plt.xlabel('Entropy')
plt.ylabel('Mass fraction')
plt.xlim(5, 20)
plt.ylim(1.e-4,1)
plt.yscale('log')
plt.legend(loc=1)

plt.savefig('./fig/hist_s.pdf')

exit()

