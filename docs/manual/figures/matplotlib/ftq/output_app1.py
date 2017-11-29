#!/usr/bin/env python3

try:
    import sys
    import numpy as np
    import matplotlib.pyplot as plt
    import argparse
except ImportError:
    print('ImportError caught. Please install matplotlib')
    exit()

import numpy as np
import matplotlib.pyplot as plt
import argparse

# Getting CLI args
parser = argparse.ArgumentParser()
parser.add_argument('--show', action='store_true', help='display the plot on screen')
parser.add_argument('--title', default='Histogram plot', help='set the title')
parser.add_argument('--eps', action='store_true', help='output .eps file')
parser.add_argument('--pdf', action='store_true', help='output .pdf file')
parser.add_argument('--png', action='store_true', help='output .png file')
parser.add_argument('--svg', action='store_true', help='output .svg file')
args = parser.parse_args()

# Parsing the data file
file_name='/home/sknigh/code/github/sst-macro/build/output_app1'
with open(file_name + '.dat') as f:
    names = f.readline().split()
    data = np.loadtxt(f, dtype=float).transpose()
    time, normalized = data[0], np.divide(data[1:-1],data[-1])

# Plot fomatting
plt.xlabel('Time (us)')
plt.xlim(time[0], time[-1])
plt.ylim(0,1)
plt.yticks([])
plt.title(args.title)
plt.legend(plt.stackplot(time, normalized), names[1:])

# Saving
if args.eps: plt.savefig(file_name + '.eps')
if args.pdf: plt.savefig(file_name + '.pdf')
if args.png: plt.savefig(file_name + '.png')
if args.svg: plt.savefig(file_name + '.svg')

if args.show:
    plt.show()
