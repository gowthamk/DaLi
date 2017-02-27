import matplotlib
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import ScalarFormatter, FormatStrFormatter


nodeKind = ['o-', 's--', 'D-.', '^:', 'X-', 'V--', '>-.', '<:']

matplotlib.rcParams['ps.fonttype'] = 42
matplotlib.rcParams['pdf.fonttype'] = 42

def main ():
  infile = open ("synchronization.dat", "r")
  tp = []
  for line in infile.readlines ():
    if line == "\n" or line.startswith ("#"):
      continue
    d = line.split ()
    tp.append ((int(d[0]), float(d[1])))

  print (tp)
  font = {'family' : 'normal', 'weight' : 'normal', 'size' : 12}
  matplotlib.rc('font', **font)

  fig, ax1 = plt.subplots()

  (tpx,tpy) = zip(*tp)
  tp, = ax1.plot (tpx,tpy, nodeKind[0], linewidth=2.0, ms=10, color='b', label="Throughput")
  ax1.set_xlabel("Synchronization frequency (ops)")
  ax1.set_ylabel("Throughput (ops/s)")
  ax1.set_ylim([0,300])
  ax1.set_xlim([1,256])
  ax1.set_xscale('log', basex=2)
  ax1.set_xticks(tpx)
  ax1.xaxis.set_major_formatter(FormatStrFormatter('%.0f'))
  ax1.grid(True)

  fig.tight_layout()
  plt.savefig ("synchronization.pdf")
  plt.close ()

main ()
