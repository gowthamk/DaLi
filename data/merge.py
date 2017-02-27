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
    merge.append ((int(d[0]), float(d[2]) * 100.0 / float(d[1]), float(d[3]) * 100.0/float(d[1])))

  font = {'family' : 'normal', 'weight' : 'normal', 'size' : 12}
  matplotlib.rc('font', **font)

  fig, ax1 = plt.subplots()

  (dist,comp,nw) = zip(*merge)

  ind = np.arange(len(dist))
  width = 0.35
  p1 = plt.bar (ind, comp, width, color='#FFCE00')
  p2 = plt.bar (ind, nw, width, color='#0375B4', bottom=comp)

  plt.xlabel("Distance (ops)")
  plt.ylabel("Time share (%)")
  plt.xticks(ind, dist)
  plt.legend((p1[0],p2[0]),('computation','network'))
  fig.tight_layout()
  plt.savefig ("merge.pdf")
  plt.close ()

main ()
