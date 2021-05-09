import sys
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import logreader

word_count_tasks = "1620537060074021903"

run = ""
if len(sys.argv) < 2:
    run = logreader.runs()[-1]
else:
    run = sys.argv[1]
lognames = logreader.lognames(run)
latest = lognames[-1]
times = logreader.load(run, latest, 'time', 'No forced failures')
times.plot('time', ['No forced failures'])
# plt.title('Tasks completed over time for word_count.shard')
plt.xlabel('Time passed (seconds)')
plt.ylabel('Tasks completed')
plt.savefig('out/{id}.png'.format(id=run))

# times = pd.read_csv('error_rate.csv.output')
# times.plot('error_rate', ['time'])
# plt.xlabel('Error rate')
# plt.ylabel('Time taken')
# plt.savefig('out2.png')

# times = pd.read_csv('times.csv.output')
# times.plot('time', ['amount'])
# plt.xlabel('Time passed (seconds)')
# plt.ylabel('Tasks completed')
# plt.savefig('out.png')