import os
import sys
import time
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import logreader

start_timestamp = 0

word_count_tasks = "1620537060074021903"

def test():
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

def ensure_cluster_setup():
    os.system("../../shard.exe ./setup.shard")

def run_shard(experiment, plot_latest, x, legend, arg=None):
    argstr = ""
    if arg is not None:
        argstr = " " + arg
    os.system("../../shard.exe ./" + experiment + ".shard" + argstr)
    run = logreader.runs()[-1]
    logname = ""
    if plot_latest:
        logname = logreader.lognames(run)[-1]
    else:
        logname = "_internal"
    return logreader.load(run, logname, x, legend)

# Memory usage, 100 trials, different clusters
def e1():
    pass

# Word count, 20 trials, different clusters
def e2():
    pass

# Word count slow, 20 trials, different clusters
def e3():
    pass

# Word length, single unreachable host in cluster, task complete time
def e4():
    ensure_cluster_setup()
    e4a = run_shard("e4a", True, "time", "No unreachable host")
    e4b = run_shard("e4b", True, "time", "One unreachable host")
    plt.plot("time", "No unreachable host", data=e4a)
    plt.plot("time", "One unreachable host", data=e4b)
    plt.xlabel('Time passed (seconds)')
    plt.ylabel('Tasks completed')
    plt.legend()
    plt.savefig('out/{ts}/e4_{ts}.png'.format(ts=start_timestamp))

# Random hash, fixed crash count, 1-5
def e5():
    ensure_cluster_setup()
    e5 = append(run_shard("e5", False, "time", "No unreachable host"))
        

# Word length, fixed crash chance, 1-50 (10 times)
def e6():
    pass

# Random hash, varying fail chance, 0-75
def e7():
    pass

# Word length, fixed fail chance, 1-50 (10 times)
def e8():
    pass

# Random hash, 10 trials, 0-75% data loss rate
def e9():
    pass

# Word length, task complete time, 0-0-0-0, 25-25-25-25, 0-0-50-50
def e10():
    pass

# Word length, 2 fast and 2 slow (10x)
def e11():
    pass

# Line length, shard vs ssh
def e12():
    pass

if __name__ == "__main__":
    start_timestamp = round(time.time() * 1000)
    os.mkdir("out/{ts}".format(ts=start_timestamp))
    # test()
    e4()

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