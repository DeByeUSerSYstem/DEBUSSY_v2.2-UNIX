import sys
import numpy as np
from numpy import sin, cos, tan, arcsin, arccos, arctan, arctan2, exp, log, log10, sqrt, degrees, radians, pi, e, sign
import matplotlib.pyplot as plt
sys.path.append(r"/Users/toscalab/DEBUSSY-2019-TOSCALAKE/BINARIES/DEBUSSY_v2.2/src/GUI/")
import debfuncx 
###################################################
plt.rc("font", family = "serif")
plt.rc("font", size = 11)
plt.rc(("xtick", "ytick"), labelsize = 11)
plt.rc("legend", fontsize = 11)
######
# XY-plot
fig = plt.figure(num = 1, figsize = (8.000000, 6.000000))
ax = plt.subplot(111)
rfln = open(r"/Users/toscalab/DISCA-TUTORIAL/Fe3O4/DFA/DB/magnetite_r006_SPH_X_Iexp.tqi", "r")
lines = rfln.readlines()
rfln.close()
lline = lines[int(len(lines)/2)].split()
ncls = len(lline)
f1 = []
f1 += [0]
for c in range(ncls):
  f1 += [np.loadtxt(r"/Users/toscalab/DISCA-TUTORIAL/Fe3O4/DFA/DB/magnetite_r006_SPH_X_Iexp.tqi", usecols = ([c]), unpack = True)]
x1 = np.loadtxt(r"/Users/toscalab/DISCA-TUTORIAL/Fe3O4/DFA/DB/magnetite_r006_SPH_X_Iexp.tqi", usecols = ([1]), unpack = True)
y1 = np.loadtxt(r"/Users/toscalab/DISCA-TUTORIAL/Fe3O4/DFA/DB/magnetite_r006_SPH_X_Iexp.tqi", usecols = ([2]), unpack = True)
ax.plot(x1, y1)
handles, labels = ax.get_legend_handles_labels()
if len(labels)>0:
    ax.legend(loc = 0)
plt.ticklabel_format(axis = "y", style = "sci", scilimits = (-1, 4))
plt.show()
