#! /usr/bin/env python

import numpy as np
import matplotlib.pyplot as plt
import sys
import math


from matplotlib.colors import LinearSegmentedColormap

def generate_cmap(colors):
    values = range(len(colors))

    vmax = np.ceil(np.max(values))
    color_list = []
    for v, c in zip(values, colors):
        color_list.append( ( v/ vmax, c) )
    return LinearSegmentedColormap.from_list('custom_cmap', color_list)



dat = sys.argv


model = '$' + dat[1] + '$-Model (' + dat[2] + 'ms)'


x_shock = []; y_shock = []
for line in open('./res/shock.dat'):
    dat = line.split()

    x_tmp = float(dat[0])
    y_tmp = float(dat[1])

    if x_tmp > 0.0:
        theta = np.arctan(x_tmp /y_tmp)
        if theta > 0.98 *math.pi /2.0:
            break

    x_shock.append(float(dat[0]))
    y_shock.append(float(dat[1]))

    if y_tmp == 0.0:
        break

x_shock.append(x_shock[-1])
y_shock.append(0.0)


File = open('./res/hydro_new.dat').readlines()

ndat = int(File[0].split()[1])

#print(ndat)

s   = [ [ 0.0 for i1 in range(ndat) ] for i2 in range(ndat) ]
ye  = [ [ 0.0 for i1 in range(ndat) ] for i2 in range(ndat) ]
bet = [ [ 0.0 for i1 in range(ndat) ] for i2 in range(ndat) ]

x = []; y = []
for i in range(1,ndat+1):
    dat = File[i].split()
    x.append(float(dat[0]))
    y.append(float(dat[0]))

y[0] = -50.0

n = 0
for j in range(ndat):
    for i in range(ndat):
        n += 1
        dat = File[n].split()
        s[j][i]   = min(float(dat[2]),30.0)
        ye[j][i]  = float(dat[3])
        bet[j][i] = float(dat[4])
    n += 1



x1, x2 = np.meshgrid(x,y)

col_range = np.linspace(0.0, 0.5, 11)
#col_range = np.linspace(0.1, 0.5, 9)



plt.rcParams['font.family']= 'Times-New-Roman'
plt.rcParams['font.size']   = 20


plt.contourf(x1, x2, ye, col_range, cmap = 'jet_r',extend = 'max')
plt.colorbar(extendfrac='auto')

plt.plot(x_shock, y_shock, 'w', linewidth = 2)

plt.text(50,1050, model)
plt.text(1075,1050,'$Y_{\\rm e}$')

plt.ylim(0,1000)

plt.xlabel('horizontal plane, km')
plt.ylabel('polar axis, km')

plt.savefig('./fig/ye.pdf')

plt.close()


col_range = np.linspace(0, 24, 17)

plt.contourf(x1, x2, s, col_range, cmap = 'hot', extend = 'max')
plt.colorbar()

plt.plot(x_shock, y_shock, 'w', linewidth = 2)

plt.text(50, 1050, model)
plt.text(1075,1050,'$S$')

plt.ylim(0,1000)

plt.xlabel('horizontal plane, km')
plt.ylabel('polar axis, km')

plt.savefig('./fig/e.pdf')
plt.close()



col_range = np.linspace(-2, 3, 11)

cm = generate_cmap(['midnightblue', 'royalblue', 'white', 'mistyrose', 'salmon', 'darkred'])

#plt.contourf(x1, x2, bet, col_range, cmap = 'Blues', extend = 'both')
plt.contourf(x1, x2, bet, col_range, cmap = cm, extend = 'both')
plt.colorbar()

plt.plot(x_shock, y_shock, 'w', linewidth = 2)

plt.text(50, 1050, model)
plt.text(1000,1050,'$\\log_{10} \\beta_{\\rm p}$')

plt.ylim(0,1000)

plt.xlabel('horizontal plane, km')
plt.ylabel('polar axis, km')

plt.savefig('./fig/bet.pdf')
plt.close()

exit()
