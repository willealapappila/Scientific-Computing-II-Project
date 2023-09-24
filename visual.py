
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Dec 18 03:12:30 2020

@author: willeala
"""

import matplotlib.pyplot as plt
import numpy as np

x, y  = [], []

for line in open('output.dat', 'r'):
    values = [float(s) for s in line.split()]
    x.append(values[0])
    y.append(values[1])
    
f = np.linspace(min(y)-2, max(y)+2, 10000)
xx = np.linspace(10,10, 10000)
    
plt.plot(x,y, color = 'darkred', label='Walked route')
plt.title('Drunken sailor random walk')
plt.plot(xx, f,'-', color='black', label='Coastline')
plt.xlabel('x-coordinate of sailor (100m)')
plt.ylabel('y-coordinate of sailor (100m)')
plt.legend()
plt.show()