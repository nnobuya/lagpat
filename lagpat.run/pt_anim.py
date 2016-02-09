#! /usr/bin/env python

import matplotlib.pyplot as plt

Test = False

stp = []; ti = []
for line in open('./res/anim_set.dat'):
    dat = line.split()
    stp.append(int(dat[0]))
    ti.append(float(dat[1]))

for n in range(len(stp)):
    x = []; y = []; ye = []
    no = '{0:04d}'.format(stp[n])

    print(no)

    for line in open('./res/anim/anim_' + no + '.dat'):
        dat = line.split()
        if dat[0] != '#':
            x.append(1.e-5 *float(dat[0]))
            y.append(1.e-5 *float(dat[1]))

    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.set_xlabel('xlabel')
    ax.set_aspect(1)

    plt.plot(x, y, 'o', c = 'b', markersize = 2, markeredgewidth = 0)
    plt.xlabel('X, km')
    plt.ylabel('Y, km')

    plt.title('Time = ' + '{0:10.1f}'.format(ti[n]) + ' ms' + '  (' + no + ')')

    plt.savefig('./anim/pt' + no + '.pdf')
    plt.close()

    if Test:
        break

exit()
