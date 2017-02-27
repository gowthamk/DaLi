import matplotlib
import numpy as np
import matplotlib.pyplot as plt

nodeKind = ['o-', 's--', 'D-.', '^:', 'X-', 'V--', '>-.', '<:']

matplotlib.rcParams['ps.fonttype'] = 42
matplotlib.rcParams['pdf.fonttype'] = 42

def main ():
  infile = open ("scalability.dat", "r")
  tp = []
  lat = []
  for line in infile.readlines ():
    if line == "\n" or line.startswith ("#"):
      continue
    d = line.split ()
    tp.append ((float(d[0]), float(d[1])))
    lat.append ((float(d[0]), float(d[2])))

  print (tp)
  print (lat)
  font = {'family' : 'normal', 'weight' : 'normal', 'size' : 12}
  matplotlib.rc('font', **font)

  fig, ax1 = plt.subplots()
  ax2 = ax1.twinx()

  (tpx,tpy) = zip(*tp)
  tp, = ax1.plot (tpx,tpy, nodeKind[0], linewidth=2.0, ms=10, color='b', label="Throughput")
  ax1.set_xlabel("# Nodes")
  ax1.set_ylabel("Throughput (ops/s)", color='b')
  ax1.tick_params('y', colors='b')
  ax1.set_ylim([0,400])
  ax1.grid(True)

  (lx,ly) = zip(*lat)
  lat, = ax2.plot (lx,ly, nodeKind[1], linewidth=2.0, ms=10, color='r', label="Latency")
  ax2_ticks = np.arange(0, 101, 25)
  ax2.set_ylabel("Latency (msec)", color='r')
  ax2.tick_params('y', colors='r')
  ax2.set_ylim([0,100])
  ax2.set_yticks(ax2_ticks)
  plt.legend(handles=[tp, lat], loc='upper left')

  fig.tight_layout()
  plt.savefig ("scalability.pdf")
  plt.close ()

main ()
