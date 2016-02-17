#! /usr/bin/env python

no_in = 1
no_ou = 123

for i in range(no_in,no_ou):
    no = '{0:05d}'.format(i + 1)
    x = []; a = []; z = []; n = []
    for line in open('./abund.in/abund_' + no + '.dat'):
        dat = line.split()
        if dat[0] == '#':
            continue
        a.append(float(dat[0]))
        z.append(float(dat[1]))
        n.append(float(dat[2]))
        x.append(float(dat[3]))

    y  = [ r1 /r2 for r1, r2 in zip(x, a) ]
    ye = 0
    for j in range(len(y)):
        ye += y[j] *z[j]

    print(ye)

exit()
