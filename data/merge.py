import matplotlib
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import ScalarFormatter, FormatStrFormatter
import matplotlib.patches as mpatches

nodeKind = ['o-', 's--', 'D-.', '^:', 'X-', 'V--', '>-.', '<:']

matplotlib.rcParams['ps.fonttype'] = 42
matplotlib.rcParams['pdf.fonttype'] = 42

def main ():
  infile = open ("merge.dat", "r")
  merge = []
  for line in infile.readlines ():
    if line == "\n" or line.startswith ("#"):
      continue
    d = line.split ()
    merge.append ((int(d[0]), float(d[1])/1000.0, float(d[2])/1000.0, float(d[3])/1000.0))

  font = {'family' : 'normal', 'weight' : 'normal', 'size' : 12}
  matplotlib.rc('font', **font)

  fig, ax1 = plt.subplots()

  (dist,merge,comp,nw) = zip(*merge)

  fig, ax = plt.subplots()
  ax.stackplot (dist, [comp, nw], colors=['#55BA87','#7E1137'])
  ax.set_xlabel("Time (s)")
  ax.set_ylabel("Distance (ops)")
  ax.set_xlim([0,256])
  plt.legend([mpatches.Patch(color='#55BA87'),
              mpatches.Patch(color='#7E1137')],
             ['computation','network'], loc='upper left')
  fig.tight_layout()
  plt.savefig ("merge.pdf")
  plt.close ()

main ()
