import matplotlib.pyplot as plt

import numpy as np
import pandas as pd

# times = pd.read_csv('times.csv.output')
# times.plot('time', ['amount'])
# plt.xlabel('Time passed (seconds)')
# plt.ylabel('Tasks completed')
# plt.savefig('out.png')

times = pd.read_csv('error_rate.csv.output')
times.plot('error_rate', ['time'])
plt.xlabel('Error rate')
plt.ylabel('Time taken')
plt.savefig('out2.png')